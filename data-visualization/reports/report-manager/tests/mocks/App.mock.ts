import { mockDeep } from 'jest-mock-extended'

import {
  DeviceService,
  ReportService,
  ReportFileService,
  ReportAttemptService,
} from 'src/app/services'
import { DeviceReports } from 'src/app/reports'
import { KafkaUtils, PrismaUtils } from 'src/utils'
import { FileManagementApi, RetrieverApi } from 'src/app/apis'
import { DeviceReportProcessor, DeviceReportQueue } from 'src/app/jobs'

export const AppMock = {
  new() {
    const KafkaUtilsMock = mockDeep<KafkaUtils>()
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
      KafkaUtilsMock,
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
