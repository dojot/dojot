import { Report } from '@prisma/client'

import { LOCALES } from 'src/app/constants'
import { DeviceReportQueue } from 'src/app/jobs'
import { AppMock, ConfigMock, LoggerMock } from 'tests/mocks'

jest.mock('bull')

describe('DeviceReport.queue', () => {
  it('should add a report to the queue', async () => {
    const { DeviceReportProcessorMock } = AppMock.new()

    const deviceReportQueue = new DeviceReportQueue(
      LoggerMock.new(),
      ConfigMock.new(),
      DeviceReportProcessorMock,
    )

    await deviceReportQueue.createReport({ id: 'id' } as Report, {
      lang: LOCALES.PT_BR,
      tenant: 'admin',
    })

    expect(deviceReportQueue['queue'].process).toBeCalled()
    expect(deviceReportQueue['queue'].add).toBeCalled()
  })
})
