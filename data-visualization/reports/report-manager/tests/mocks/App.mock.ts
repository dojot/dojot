import { mockDeep } from 'jest-mock-extended'

import {
  DeviceService,
  ReportService,
  ReportFileService,
  ReportAttemptService,
} from 'src/app/services'
import { PrismaUtils } from 'src/utils'
import { DeviceReports } from 'src/app/reports'
import { FileManagementApi, RetrieverApi } from 'src/app/apis'
import { DeviceReportProcessor, DeviceReportQueue } from 'src/app/jobs'

export const AppMock = {
  new() {
    const PrismaUtilsMock = mockDeep<PrismaUtils>()

    const DeviceReportsMock = mockDeep<DeviceReports>()

    const DeviceServiceMock = mockDeep<DeviceService>()
    const ReportServiceMock = mockDeep<ReportService>()
    const ReportFileServiceMock = mockDeep<ReportFileService>()
    const ReportAttemptServiceMock = mockDeep<ReportAttemptService>()

    const RetrieverApiMock = mockDeep<RetrieverApi>()
    const FileManagementApiMock = mockDeep<FileManagementApi>()

    const DeviceReportQueueMock = mockDeep<DeviceReportQueue>()
    const DeviceReportProcessorMock = mockDeep<DeviceReportProcessor>()

    return {
      PrismaUtilsMock,

      DeviceReportsMock,

      DeviceServiceMock,
      ReportServiceMock,
      ReportFileServiceMock,
      ReportAttemptServiceMock,

      RetrieverApiMock,
      FileManagementApiMock,

      DeviceReportQueueMock,
      DeviceReportProcessorMock,
    }
  },
}
