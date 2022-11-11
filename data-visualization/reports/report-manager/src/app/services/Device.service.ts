import { PrismaClient } from '@prisma/client'

import { REPORT_TYPES } from 'src/app/constants'
import { DeviceReportQueue } from 'src/app/jobs'
import { CreateDeviceReportDto } from 'src/app/dto'

type CreateReportMetadata = {
  lang: string
  tenant: string
}

export class DeviceService {
  constructor(private deviceReportQueue: DeviceReportQueue) {}

  async create(
    prisma: PrismaClient,
    dto: CreateDeviceReportDto,
    metadata: CreateReportMetadata,
  ) {
    const type = await prisma.reportType.findUnique({
      where: { identifier: REPORT_TYPES.DEVICES },
    })

    if (!type) throw new Error('Report type not found')

    const report = await prisma.report.create({
      data: {
        name: dto.name,
        typeId: type.id,
        format: dto.format,
        finalDate: dto.finalDate,
        initialDate: dto.initialDate,
        params: JSON.stringify(dto.devices),
        singleReportFile: dto.singleReportFile ?? true,
      },
    })

    await this.deviceReportQueue.createReport(report, {
      lang: metadata.lang,
      tenant: metadata.tenant,
    })

    return report
  }
}
