import { ReportAttempt } from '@prisma/client'

import { DeviceReportProcessor } from 'src/app/jobs'
import { LOCALES, REPORT_FORMATS } from 'src/app/constants'
import {
  AppMock,
  ConfigMock,
  LoggerMock,
  BullJobMock,
  PrismaClientMock,
  KeycloakClientSessionMock,
} from 'tests/mocks'

describe('DeviceReport.processor', () => {
  const fakeParams = [
    {
      id: '05af2',
      label: 'dev',
      attrs: [
        {
          id: 1,
          label: 'attr',
          type: 'dynamic',
          valueType: 'integer',
        },
      ],
    },
  ]

  it('should process a CSV report', async () => {
    const {
      PrismaUtilsMock,
      RetrieverApiMock,
      FileManagementApiMock,
      DeviceReportsMock,
      ReportAttemptServiceMock,
      ReportFileServiceMock,
    } = AppMock.new()

    RetrieverApiMock.getDeviceData.mockResolvedValue({
      data: [
        {
          ts: new Date().toISOString(),
          attrs: [{ label: 'attr', value: '20' }],
        },
        {
          ts: new Date().toISOString(),
          attrs: [{ label: 'attr', value: '10' }],
        },
      ],
      paging: {
        current: { number: 1, url: '' },
        next: null,
        previous: null,
      },
    })

    FileManagementApiMock.upload.mockResolvedValue({
      details: {
        encoding: '7bit',
        filename: 'file.csv',
        mimetype: 'text/csv',
        transactionCode: '1',
        info: {
          etag: 'tag',
          versionId: 'id',
        },
      },
      message: 'file uploaded',
    })

    ReportAttemptServiceMock.create.mockResolvedValue({
      id: 'attempt',
    } as ReportAttempt)

    DeviceReportsMock.createCsv.mockReturnValue(Buffer.from([]))

    ReportFileServiceMock.create.mockResolvedValue({
      id: 'id',
      reportId: 'reportId',
      mimeType: 'text/csv',
      filename: 'file.csv',
      path: '/reports/file.csv',
      fileSizeKb: 1024,
      expiresAt: null,
      createdAt: new Date(),
      updatedAt: new Date(),
    })

    const deviceReportProcessor = new DeviceReportProcessor(
      LoggerMock.new(),
      ConfigMock.new(),
      PrismaUtilsMock,
      RetrieverApiMock,
      FileManagementApiMock,
      DeviceReportsMock,
      ReportAttemptServiceMock,
      ReportFileServiceMock,
    )

    const FakeKeycloakSession = KeycloakClientSessionMock.new()
    const FakePrismaClient = PrismaClientMock.new()

    FakeKeycloakSession.getTokenSet.mockReturnValue({
      access_token: 'token',
    } as never)

    const newPrismaClientMock = jest
      .spyOn(DeviceReportProcessor.prototype as never, 'newPrismaClient')
      .mockReturnValue(FakePrismaClient as never)

    const newKeycloakClientSessionMock = jest
      .spyOn(
        DeviceReportProcessor.prototype as never,
        'newKeycloakClientSession',
      )
      .mockReturnValue(FakeKeycloakSession as never)

    await deviceReportProcessor.process(
      BullJobMock.new({
        lang: LOCALES.PT_BR,
        tenant: 'admin',
        report: {
          id: 'id',
          typeId: 'typeId',
          name: 'Report',
          singleReportFile: true,
          format: REPORT_FORMATS.CSV,
          params: JSON.stringify(fakeParams),
          createdAt: new Date().toISOString(),
          updatedAt: new Date().toISOString(),
          finalDate: null,
          initialDate: null,
        },
      }),
    )

    // Calls in sequence
    expect(newPrismaClientMock).toBeCalled()
    expect(ReportAttemptServiceMock.create).toBeCalled()
    expect(newKeycloakClientSessionMock).toBeCalled()
    expect(FakeKeycloakSession.start).toBeCalled()
    expect(FakeKeycloakSession.getTokenSet).toBeCalled()
    expect(RetrieverApiMock.getDeviceData).toBeCalled()
    expect(DeviceReportsMock.setLocale).toBeCalled()
    expect(DeviceReportsMock.createCsv).toBeCalled()
    expect(FileManagementApiMock.upload).toBeCalled()
    expect(ReportAttemptServiceMock.update).toBeCalled()
    expect(ReportFileServiceMock.create).toBeCalled()
    expect(FakeKeycloakSession.close).toBeCalled()
  })

  it('should forward error in catch block', () => {
    const {
      PrismaUtilsMock,
      RetrieverApiMock,
      FileManagementApiMock,
      DeviceReportsMock,
      ReportAttemptServiceMock,
      ReportFileServiceMock,
    } = AppMock.new()

    ReportAttemptServiceMock.create.mockResolvedValue({
      id: 'attempt',
    } as ReportAttempt)

    const deviceReportProcessor = new DeviceReportProcessor(
      LoggerMock.new(),
      ConfigMock.new(),
      PrismaUtilsMock,
      RetrieverApiMock,
      FileManagementApiMock,
      DeviceReportsMock,
      ReportAttemptServiceMock,
      ReportFileServiceMock,
    )

    const FakeKeycloakSession = KeycloakClientSessionMock.new()
    const FakePrismaClient = PrismaClientMock.new()

    FakeKeycloakSession.getTokenSet.mockReturnValue({
      access_token: 'token',
    } as never)

    jest
      .spyOn(DeviceReportProcessor.prototype as never, 'newPrismaClient')
      .mockReturnValue(FakePrismaClient as never)

    jest
      .spyOn(
        DeviceReportProcessor.prototype as never,
        'newKeycloakClientSession',
      )
      .mockReturnValue(new Error('Error') as never)

    const fn = () => {
      return deviceReportProcessor.process(
        BullJobMock.new({
          lang: LOCALES.PT_BR,
          tenant: 'admin',
          report: {
            id: 'id',
            typeId: 'typeId',
            name: 'Report',
            singleReportFile: true,
            format: REPORT_FORMATS.CSV,
            params: JSON.stringify(fakeParams),
            createdAt: new Date().toISOString(),
            updatedAt: new Date().toISOString(),
            finalDate: null,
            initialDate: null,
          },
        }),
      )
    }

    expect(fn).rejects.toThrow()
  })
})
