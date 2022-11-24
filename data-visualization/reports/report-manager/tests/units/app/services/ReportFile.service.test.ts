import { PrismaClientMock } from 'tests/mocks'
import { ReportFileService } from 'src/app/services'

describe('ReportFile.service', () => {
  const reportFileService = new ReportFileService()

  describe('create', () => {
    it('should create a report attempt', async () => {
      const FakePrismaClient = PrismaClientMock.new()

      await reportFileService.create(FakePrismaClient, {
        reportId: 'id',
        fileSizeKb: 1024,
        mimeType: 'text/csv',
        filename: 'file.csv',
        path: '/reports/file.csv',
        expiresAt: new Date().toISOString(),
      })

      expect(FakePrismaClient.reportFile.create).toBeCalled()
    })
  })
})
