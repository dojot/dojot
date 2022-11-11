import { Report, ReportFile } from '@prisma/client'
import { PrismaClientKnownRequestError } from '@prisma/client/runtime'

import { AppMock, LoggerMock, PrismaClientMock } from 'tests/mocks'
import { ReportService } from 'src/app/services'
import { PRISMA_ERRORS } from 'src/app/constants'

describe('Report.service', () => {
  const { FileManagementApiMock } = AppMock.new()
  const reportService = new ReportService(
    LoggerMock.new(),
    FileManagementApiMock,
  )

  describe('count', () => {
    it('should return the number of reports', async () => {
      const FakePrismaClient = PrismaClientMock.new()
      FakePrismaClient.report.count.mockResolvedValue(155)
      const count = await reportService.count(FakePrismaClient)
      expect(count).toBe(155)
    })
  })

  describe('findMany', () => {
    it('should find no reports and return an empty array', async () => {
      const FakePrismaClient = PrismaClientMock.new()
      FakePrismaClient.report.findMany.mockResolvedValue([])
      FakePrismaClient.report.count.mockResolvedValue(0)
      const { reports, total } = await reportService.findMany(FakePrismaClient)
      expect(FakePrismaClient.report.findMany).toBeCalled()
      expect(reports).toHaveLength(0)
      expect(total).toBe(0)
    })

    it('should parse the params of the reports array', async () => {
      const FakePrismaClient = PrismaClientMock.new()

      const fakeReport1 = { params: '{ "test": "test" }' } as Report
      const fakeReport2 = {} as Report
      const fakeReports = [fakeReport1, fakeReport2]

      FakePrismaClient.report.findMany.mockResolvedValue(fakeReports)
      FakePrismaClient.report.count.mockResolvedValue(fakeReports.length)

      const { reports, total } = await reportService.findMany(FakePrismaClient)

      expect(FakePrismaClient.report.findMany).toBeCalled()
      expect(reports[0].params).toEqual({ test: 'test' })
      expect(reports[1].params).toBeFalsy()
      expect(total).toBe(fakeReports.length)
    })

    it('should use page and pageSize params', async () => {
      const FakePrismaClient = PrismaClientMock.new()
      FakePrismaClient.report.findMany.mockResolvedValue([])
      await reportService.findMany(FakePrismaClient, { page: 2, pageSize: 15 })
      expect(FakePrismaClient.report.findMany).toBeCalledWith(
        expect.objectContaining({
          skip: 15,
          take: 15,
        }),
      )
    })

    it('should search reports by name', async () => {
      const name = 'Report'
      const FakePrismaClient = PrismaClientMock.new()
      FakePrismaClient.report.findMany.mockResolvedValue([])
      await reportService.findMany(FakePrismaClient, { name })
      expect(FakePrismaClient.report.findMany).toBeCalledWith(
        expect.objectContaining({
          where: {
            name: {
              contains: name,
              mode: 'insensitive',
            },
          },
        }),
      )
    })
  })

  describe('findById', () => {
    it('should not find the report by id', async () => {
      const FakePrismaClient = PrismaClientMock.new()
      const report = await reportService.findById(FakePrismaClient, 'id')
      expect(FakePrismaClient.report.findUnique).toBeCalled()
      expect(report).toBeFalsy()
    })

    it('should find a report and parse params', async () => {
      const FakePrismaClient = PrismaClientMock.new()
      const fakeReport = { params: '{ "test": "test" }' } as Report
      FakePrismaClient.report.findUnique.mockResolvedValue(fakeReport)
      const report = await reportService.findById(FakePrismaClient, 'id')
      expect(FakePrismaClient.report.findUnique).toBeCalled()
      expect(report?.params).toEqual({ test: 'test' })
    })
  })

  describe('delete', () => {
    it('should not find the report by id to delete', async () => {
      const FakePrismaClient = PrismaClientMock.new()
      const report = await reportService.delete(FakePrismaClient, 'id', 'token')
      expect(FakePrismaClient.report.delete).toBeCalled()
      expect(report).toBeFalsy()
    })

    it('should return a falsy value when handle a prisma known error', async () => {
      const prismaError = new PrismaClientKnownRequestError(
        'Prisma Error',
        PRISMA_ERRORS.DEPENDS_ON_NOT_FOUND_RECORD,
        'x.x.x',
      )

      const FakePrismaClient = PrismaClientMock.new()
      FakePrismaClient.report.delete.mockRejectedValue(prismaError)
      const report = await reportService.delete(FakePrismaClient, 'id', 'token')

      expect(FakePrismaClient.report.delete).toBeCalled()
      expect(report).toBeFalsy()
    })

    it('should throw when an unknown known error occurs', async () => {
      const error = new Error('Unknown Error')
      const FakePrismaClient = PrismaClientMock.new()
      FakePrismaClient.report.delete.mockRejectedValue(error)

      const fn = async () => {
        return reportService.delete(FakePrismaClient, 'id', 'token')
      }

      expect(fn).rejects.toThrowError(error)
    })

    it('should delete the report and parse params', async () => {
      const fakeReport = { id: 'id', params: '{ "test": "test" }' } as Report

      const FakePrismaClient = PrismaClientMock.new()
      FakePrismaClient.report.delete.mockResolvedValue(fakeReport)

      const { FileManagementApiMock } = AppMock.new()
      const reportService = new ReportService(
        LoggerMock.new(),
        FileManagementApiMock,
      )

      const report = await reportService.delete(FakePrismaClient, 'id', 'token')

      expect(FakePrismaClient.report.delete).toBeCalled()
      expect(FileManagementApiMock.delete).not.toBeCalled()
      expect(report).toBeTruthy()
      expect(report?.params).toEqual({ test: 'test' })
    })

    it('should delete the report and make request to delete file', async () => {
      const fakeReport = {
        id: 'id',
        file: { path: 'path/to/file' },
      } as Report & { file: ReportFile }

      const { FileManagementApiMock } = AppMock.new()
      FileManagementApiMock.delete.mockReturnValue(Promise.resolve({} as never))

      const reportService = new ReportService(
        LoggerMock.new(),
        FileManagementApiMock,
      )

      const FakePrismaClient = PrismaClientMock.new()
      FakePrismaClient.report.delete.mockResolvedValue(fakeReport)
      const report = await reportService.delete(FakePrismaClient, 'id', 'token')

      expect(FakePrismaClient.report.delete).toBeCalled()
      expect(FileManagementApiMock.delete).toBeCalled()
      expect(report).toBeTruthy()
    })

    it('should handle errors when the request to delete file fails', async () => {
      const fakeReport = {
        id: 'id',
        file: { path: 'path/to/file' },
      } as Report & { file: ReportFile }

      const { FileManagementApiMock } = AppMock.new()
      const error = new Error('Error')
      FileManagementApiMock.delete.mockRejectedValue(error)

      const FakeLogger = LoggerMock.new()
      const reportService = new ReportService(FakeLogger, FileManagementApiMock)

      const FakePrismaClient = PrismaClientMock.new()
      FakePrismaClient.report.delete.mockResolvedValue(fakeReport)
      const report = await reportService.delete(FakePrismaClient, 'id', 'token')

      expect(FakeLogger.debug).toBeCalledWith(expect.any(String), error)
      expect(FakePrismaClient.report.delete).toBeCalled()
      expect(FileManagementApiMock.delete).toBeCalled()
      expect(report).toBeTruthy()
    })
  })
})
