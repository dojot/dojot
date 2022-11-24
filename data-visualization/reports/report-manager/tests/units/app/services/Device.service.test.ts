import { ReportType } from '@prisma/client'

import { AppMock, PrismaClientMock } from 'tests/mocks'
import { DeviceService } from 'src/app/services'
import { LOCALES, REPORT_FORMATS, REPORT_TYPES } from 'src/app/constants'

describe('Device.service', () => {
  const { DeviceReportQueueMock } = AppMock.new()
  const deviceService = new DeviceService(DeviceReportQueueMock)

  describe('create', () => {
    const devices = [
      {
        id: 'id',
        label: 'label',
        attrs: [
          {
            id: 1,
            label: 'label',
            type: 'dynamic',
            valueType: 'float',
          },
        ],
      },
    ]

    const reportType: ReportType = {
      id: 'id',
      name: 'Devices',
      identifier: REPORT_TYPES.DEVICES,
      createdAt: new Date(),
      updatedAt: new Date(),
    }

    it('should create a device report record', async () => {
      const FakePrismaClient = PrismaClientMock.new()
      FakePrismaClient.reportType.findUnique.mockResolvedValue(reportType)

      await deviceService.create(
        FakePrismaClient,
        {
          devices,
          name: 'Report',
          singleReportFile: true,
          format: REPORT_FORMATS.CSV,
        },
        {
          tenant: 'test',
          lang: LOCALES.PT_BR,
        },
      )

      expect(FakePrismaClient.reportType.findUnique).toBeCalled()
      expect(FakePrismaClient.report.create).toBeCalled()
    })

    it('should use the singleReportFile default value', async () => {
      const FakePrismaClient = PrismaClientMock.new()
      FakePrismaClient.reportType.findUnique.mockResolvedValue(reportType)

      FakePrismaClient.report.create.mockImplementation(
        ({ data }) => Promise.resolve(data) as never,
      )

      const report = await deviceService.create(
        FakePrismaClient,
        {
          devices,
          name: 'Report',
          format: REPORT_FORMATS.CSV,
        },
        {
          tenant: 'test',
          lang: LOCALES.PT_BR,
        },
      )

      expect(report.singleReportFile).toBe(true)
    })

    it('should throw error if do not find the report type', async () => {
      const FakePrismaClient = PrismaClientMock.new()

      const fn = () => {
        return deviceService.create(
          FakePrismaClient,
          {
            devices,
            name: 'Report',
            singleReportFile: true,
            format: REPORT_FORMATS.CSV,
          },
          {
            tenant: 'test',
            lang: LOCALES.PT_BR,
          },
        )
      }

      expect(fn).rejects.toThrow(Error)
    })
  })
})
