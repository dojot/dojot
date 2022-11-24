import { Logger } from '@dojot/microservice-sdk'
import { Prisma, PrismaClient } from '@prisma/client'
import { PrismaClientKnownRequestError } from '@prisma/client/runtime'

import { FileManagementApi } from 'src/app/apis'
import { PRISMA_ERRORS } from 'src/app/constants'

type FindManyParams = {
  name?: string
  page?: number
  pageSize?: number
}

export class ReportService {
  constructor(
    private logger: Logger,
    private fileManagementApi: FileManagementApi,
  ) {}

  private parseParams(params?: Prisma.JsonValue) {
    return params ? JSON.parse(String(params)) : undefined
  }

  async count(prisma: PrismaClient) {
    return prisma.report.count()
  }

  async findMany(
    prisma: PrismaClient,
    { name, page, pageSize }: FindManyParams = {},
  ) {
    const skip = page && pageSize ? page * pageSize - pageSize : undefined

    const reports = await prisma.report.findMany({
      skip,
      take: pageSize,
      where: {
        name: {
          contains: name,
          mode: 'insensitive',
        },
      },
      include: {
        file: true,
        type: true,
        attempts: {
          orderBy: {
            createdAt: 'desc',
          },
        },
      },
    })

    const reportsWithParsedParams = reports.map((report) => {
      return { ...report, params: this.parseParams(report.params) }
    })

    const total = await this.count(prisma)

    return { reports: reportsWithParsedParams, total }
  }

  async findById(prisma: PrismaClient, id: string) {
    const report = await prisma.report.findUnique({
      where: { id },
      include: {
        file: true,
        type: true,
        attempts: {
          orderBy: {
            createdAt: 'desc',
          },
        },
      },
    })

    if (report) report.params = this.parseParams(report.params)
    return report
  }

  async delete(prisma: PrismaClient, id: string, token: string) {
    try {
      const deletedReport = await prisma.report.delete({
        where: { id },
        include: {
          file: true,
          type: true,
          attempts: true,
        },
      })

      if (deletedReport) {
        deletedReport.params = this.parseParams(deletedReport.params)

        const filePath = deletedReport.file?.path
        if (filePath) {
          await this.fileManagementApi
            .delete(filePath, { token })
            .catch((e) => {
              this.logger.debug('delete: Failed to delete file', e as never)
            })
        }
      }

      return deletedReport
    } catch (e) {
      if (
        e instanceof PrismaClientKnownRequestError &&
        e.code === PRISMA_ERRORS.DEPENDS_ON_NOT_FOUND_RECORD
      ) {
        return null
      }

      throw e
    }
  }
}
