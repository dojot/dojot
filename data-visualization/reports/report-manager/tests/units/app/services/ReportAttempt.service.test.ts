import { PrismaClientMock } from 'tests/mocks'
import { ReportAttemptService } from 'src/app/services'

describe('ReportAttempt.service', () => {
  const reportAttemptService = new ReportAttemptService()

  describe('create', () => {
    it('should create a report attempt', async () => {
      const FakePrismaClient = PrismaClientMock.new()
      await reportAttemptService.create(FakePrismaClient, { reportId: 'id' })
      expect(FakePrismaClient.reportAttempt.create).toBeCalled()
    })
  })

  describe('update', () => {
    it('should update a report attempt', async () => {
      const FakePrismaClient = PrismaClientMock.new()

      await reportAttemptService.update(FakePrismaClient, 'id', {
        error: 'error',
        failedAt: new Date().toISOString(),
        finishedAt: new Date().toISOString(),
        canceledAt: new Date().toISOString(),
      })

      expect(FakePrismaClient.reportAttempt.update).toBeCalled()
    })
  })
})
