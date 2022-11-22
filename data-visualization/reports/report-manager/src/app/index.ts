import { Express } from 'express'
import { Logger, ServiceStateManager, WebUtils } from '@dojot/microservice-sdk'

import {
  LanguageInterceptor,
  PrismaClientInterceptor,
  DefaultErrorHandlerInterceptor,
} from 'src/app/interceptors'
import {
  DeviceService,
  ReportService,
  ReportFileService,
  ReportAttemptService,
} from 'src/app/services'
import { Config } from 'src/types'
import { PrismaUtils } from 'src/utils'
import { DeviceReports } from 'src/app/reports'
import { LocaleManager } from 'src/app/locales'
import { SERVICE_NAMES } from 'src/app/constants'
import { KafkaConsumer, TenantManager } from 'src/kafka'
import { DeviceRoutes, ReportRoutes } from 'src/app/routes'
import { FileManagementApi, RetrieverApi } from 'src/app/apis'
import { DeviceReportProcessor, DeviceReportQueue } from 'src/app/jobs'

import { DeviceController, ReportController } from './controllers'

export class App {
  constructor(
    private logger: Logger,
    private config: Config,
    private prismaUtils: PrismaUtils,
    private kafkaConsumer: KafkaConsumer,
    private tenantManager: TenantManager,
    private serviceState: ServiceStateManager,
    private retrieverHttpClient: WebUtils.DojotHttpClient,
    private fileManagementHttpClient: WebUtils.DojotHttpClient,
  ) {}

  private createExpress(): Express {
    const { createKeycloakAuthInterceptor, jsonBodyParsingInterceptor } =
      WebUtils.framework.interceptors

    const retrieverApi = new RetrieverApi(this.retrieverHttpClient)
    const fileManagementApi = new FileManagementApi(
      this.fileManagementHttpClient,
    )

    const localeManager = new LocaleManager(this.logger)
    const deviceReports = new DeviceReports(this.logger, localeManager)

    const reportAttemptService = new ReportAttemptService()
    const reportFileService = new ReportFileService()

    const deviceReportProcessor = new DeviceReportProcessor(
      this.logger,
      this.config,
      this.prismaUtils,
      retrieverApi,
      fileManagementApi,
      deviceReports,
      reportAttemptService,
      reportFileService,
    )

    const deviceReportQueue = new DeviceReportQueue(
      this.logger,
      this.config,
      deviceReportProcessor,
    )

    const deviceService = new DeviceService(deviceReportQueue)
    const deviceController = new DeviceController(this.logger, deviceService)

    const reportService = new ReportService(this.logger, fileManagementApi)
    const reportController = new ReportController(this.logger, reportService)

    return WebUtils.framework.createExpress({
      server: undefined,
      logger: this.logger,
      supportWebsockets: false,
      supportTrustProxy: false,
      catchInvalidRequest: false,
      errorHandlers: [
        DefaultErrorHandlerInterceptor.use(this.prismaUtils),
        WebUtils.framework.defaultErrorHandler({
          logger: this.logger,
        }),
      ],
      interceptors: [
        jsonBodyParsingInterceptor({
          config: {
            limit: this.config.express['parsing.limit'],
          },
        }),
        createKeycloakAuthInterceptor(
          this.tenantManager.tenants,
          this.logger,
          '/',
        ),
        PrismaClientInterceptor.use(this.logger, this.prismaUtils),
        LanguageInterceptor.use(this.logger),
      ],
      routes: [
        DeviceRoutes.use(this.prismaUtils, deviceController),
        ReportRoutes.use(this.prismaUtils, reportController),
      ].flat(),
    })
  }

  async init() {
    await this.kafkaConsumer.init()
    await this.tenantManager.update()

    this.kafkaConsumer.registerTenantEvent(
      this.tenantManager.handleTenantEvent.bind(this.tenantManager),
    )

    this.serviceState.registerService(SERVICE_NAMES.REPORT_MANAGER)

    const express = this.createExpress()
    const server = express.listen(this.config.server.port)

    server.on('listening', () => {
      this.logger.info('Server is ready to accept connections', {})
      this.logger.info(`Running at port: ${this.config.server.port}`, {})
      this.serviceState.signalReady(SERVICE_NAMES.REPORT_MANAGER)
    })

    server.on('close', () => {
      this.logger.info('Server will be closed', {})
      this.serviceState.signalNotReady(SERVICE_NAMES.REPORT_MANAGER)
    })

    server.on('error', () => {
      this.logger.info('Received an error event', {})
      this.serviceState.signalNotReady(SERVICE_NAMES.REPORT_MANAGER)
    })

    return server
  }
}
