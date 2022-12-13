import JSZip from 'jszip'
import { Job } from 'bull'
import { PrismaClient, Report } from '@prisma/client'
import { Logger, WebUtils } from '@dojot/microservice-sdk'

import { Config } from 'src/types'
import { PrismaUtils } from 'src/utils'
import { DevicesParamsDto } from 'src/app/dto'
import { DeviceReports } from 'src/app/reports'
import { REPORT_FORMATS } from 'src/app/constants'
import { FileManagementApi, RetrieverApi } from 'src/app/apis'
import { ReportAttemptService, ReportFileService } from 'src/app/services'

type ReportData = Omit<
  Report,
  'initialDate' | 'finalDate' | 'createdAt' | 'updatedAt'
> & {
  initialDate?: string | null
  finalDate?: string | null
  createdAt: string
  updatedAt: string
}

type DeviceReportJobData = {
  lang: string
  tenant: string
  report: ReportData
}

interface ReportDataPerDevice {
  deviceId: string
  deviceLabel: string
  attrs: Array<{
    timestamp: string
    label: string
    value: string | number
  }>
}

const KILOBYTE_IN_BYTES = 1024

export class DeviceReportProcessor {
  constructor(
    private logger: Logger,
    private config: Config,
    private prismaUtils: PrismaUtils,
    private retrieverApi: RetrieverApi,
    private fileManagementApi: FileManagementApi,
    private deviceReports: DeviceReports,
    private reportAttemptService: ReportAttemptService,
    private reportFileService: ReportFileService,
  ) {
    this.logger.debug('constructor: device report processor instantiated', {})
  }

  private newPrismaClient(tenant: string) {
    this.logger.debug('newPrismaClient: creating prisma client', { tenant })
    const databaseUrl = this.prismaUtils.getDatabaseUrl(tenant)
    return new PrismaClient({ datasources: { db: { url: databaseUrl } } })
  }

  private newKeycloakClientSession(tenant: string) {
    this.logger.debug(
      'newKeycloakClientSession: creating keycloak client session',
      { tenant },
    )

    return new WebUtils.KeycloakClientSession(
      this.config.keycloak.url,
      tenant,
      {
        grant_type: 'client_credentials',
        client_id: this.config.keycloak['client.id'],
        client_secret: this.config.keycloak['client.secret'],
      },
      this.logger,
      { retryDelay: 5000 },
    )
  }

  private parseParams(params: string): DevicesParamsDto[] {
    return JSON.parse(params)
  }

  private async getAllDeviceData(
    report: ReportData,
    deviceId: string,
    token: string,
  ) {
    const initialDateISO = report.initialDate
      ? new Date(report.initialDate).toISOString()
      : undefined

    const finalDateISO = report.finalDate
      ? new Date(report.finalDate).toISOString()
      : undefined

    const data = []
    let hasNextPage = true
    let page = 1

    while (hasNextPage) {
      this.logger.debug(`getAllDeviceData: getting page - ${page}`, {})

      const deviceData = await this.retrieverApi.getDeviceData(
        deviceId,
        { token },
        {
          page,
          order: 'desc',
          dateTo: finalDateISO,
          dateFrom: initialDateISO,
        },
      )

      data.push(deviceData.data)

      const nextPage = deviceData.paging.next
      page = nextPage?.number || 0
      hasNextPage = !!nextPage
    }

    return data.flat()
  }

  private async getDeviceReportData(
    params: DevicesParamsDto[],
    report: ReportData,
    token: string,
  ) {
    this.logger.debug(
      'getDeviceReportData: getting device data from retriever',
      {},
    )

    const reportData: ReportDataPerDevice[] = []

    for (const param of params) {
      this.logger.debug(`getDeviceReportData: device ID = ${param.id}`, {})

      const deviceDataAttrs: ReportDataPerDevice['attrs'] = []

      const allDeviceData = await this.getAllDeviceData(report, param.id, token)
      const attrLabelsToFilter = param.attrs.map(({ label }) => label)

      allDeviceData.forEach(({ ts, attrs }) => {
        attrs.forEach(({ label, value }) => {
          if (attrLabelsToFilter.includes(label)) {
            deviceDataAttrs.push({ timestamp: ts, label, value })
          }
        })
      })

      reportData.push({
        deviceId: param.id,
        deviceLabel: param.label,
        attrs: deviceDataAttrs,
      })
    }

    this.logger.debug(
      'getDeviceReportData: device data has been retrieved and parsed',
      {},
    )

    return reportData
  }

  private getFileName(report: ReportData): string {
    this.logger.debug('getFileName: creating filename for report', {})
    const { id, singleReportFile, format } = report
    if (singleReportFile) return `${id}.${format}`
    return `${id}.zip`
  }

  private getFilePath(filename: string): string {
    this.logger.debug('getFilePath: creating path for report', { filename })
    const path = this.config.app['report.path']
    return `${path}/${filename}`
  }

  private getDataForCsv(reportDataPerDevice: ReportDataPerDevice[]) {
    this.logger.debug('getDataForCsv: parsing data to create a csv report', {})

    const dataForCsv = reportDataPerDevice.map(
      ({ deviceId, deviceLabel, attrs }) => {
        return attrs.map(({ timestamp, value, label }) => {
          return { deviceId, deviceLabel, timestamp, value, attr: label }
        })
      },
    )

    return dataForCsv.flat()
  }

  private async getReportBuffer(
    format: string,
    reportDataPerDevice: ReportDataPerDevice[],
  ): Promise<Buffer> {
    this.logger.debug('getReportBuffer: creating report buffer', { format })

    if (format === REPORT_FORMATS.CSV) {
      return this.deviceReports.createCsv(
        this.getDataForCsv(reportDataPerDevice),
      )
    }

    if (format === REPORT_FORMATS.PDF) {
      return await this.deviceReports.createPdf(reportDataPerDevice)
    }

    throw new Error('Report format not supported')
  }

  private async getFileBuffer(
    report: ReportData,
    reportDataPerDevice: ReportDataPerDevice[],
  ): Promise<Buffer> {
    if (report.singleReportFile) {
      this.logger.debug('getFileBuffer: creating a single report file', {})
      return this.getReportBuffer(report.format, reportDataPerDevice)
    }

    this.logger.debug('getFileBuffer: creating multiple report files', {})

    const zip = new JSZip()
    for (const deviceData of reportDataPerDevice) {
      const buffer = await this.getReportBuffer(report.format, [deviceData])
      zip.file(`${deviceData.deviceId}.${report.format}`, buffer)
    }

    this.logger.debug('getFileBuffer: zipping report files', {})

    return zip.generateAsync({ type: 'nodebuffer' })
  }

  private getExpirationDate(): string | null {
    this.logger.debug('getExpirationDate: getting expiration date', {})

    const defaultExpirationMs = this.config.app['report.expiration.ms']
    if (defaultExpirationMs === 0) {
      this.logger.debug('getExpirationDate: expiration not defined', {})
      return null
    }

    const expirationDate = new Date(Date.now() + defaultExpirationMs)

    this.logger.debug('getExpirationDate: expiration defined', {
      defaultExpirationMs,
      expirationDate: expirationDate.toISOString(),
    })

    return expirationDate.toISOString()
  }

  async process(job: Job<DeviceReportJobData>) {
    const { lang, tenant, report } = job.data

    this.logger.debug('process: starting to process job', {
      lang,
      tenant,
      jobId: job.id,
      reportId: report.id,
    })

    const prisma = this.newPrismaClient(tenant)

    const reportAttempt = await this.reportAttemptService.create(prisma, {
      reportId: report.id,
    })

    this.logger.debug('process: attempt created', {
      attemptId: reportAttempt.id,
    })

    try {
      const keycloakClientSession = this.newKeycloakClientSession(tenant)
      await keycloakClientSession.start()

      const tokenSet = keycloakClientSession.getTokenSet()
      const token = tokenSet.access_token || ''

      const params = this.parseParams(String(report.params))
      const reportData = await this.getDeviceReportData(params, report, token)

      this.deviceReports.setLocale(lang)
      const fileBuffer = await this.getFileBuffer(report, reportData)
      const filename = this.getFileName(report)
      const path = this.getFilePath(filename)

      this.logger.debug('process: uploading file to file management', {})

      const file = await this.fileManagementApi.upload(
        { file: fileBuffer, path },
        { token },
      )

      this.logger.debug('process: upload successfully', {})
      this.logger.debug('process: updating attempt finishedAt date', {})

      await this.reportAttemptService.update(prisma, reportAttempt.id, {
        finishedAt: new Date().toISOString(),
      })

      this.logger.debug('process: attempt finishedAt date updated', {})
      this.logger.debug('process: saving report file record', {})

      const reportFile = await this.reportFileService.create(prisma, {
        path,
        filename: filename,
        reportId: report.id,
        mimeType: file.details.mimetype,
        expiresAt: this.getExpirationDate(),
        fileSizeKb: fileBuffer.byteLength / KILOBYTE_IN_BYTES,
      })

      this.logger.debug('process: report file record saved', {
        reportFileId: reportFile.id,
      })

      keycloakClientSession.close()
    } catch (e) {
      this.logger.error('process: failed to process job', e as never)

      const errorJson = JSON.stringify({ message: (e as Error).message })

      this.logger.debug('process: updating attempt failedAt date', {})

      await this.reportAttemptService.update(prisma, reportAttempt.id, {
        error: errorJson,
        failedAt: new Date().toISOString(),
      })

      this.logger.debug('process: attempt failedAt date updated', {})

      throw e
    }
  }
}
