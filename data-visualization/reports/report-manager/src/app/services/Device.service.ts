import { PrismaClient } from '@prisma/client'

import { REPORT_TYPES } from 'src/app/constants'
import { CreateDeviceReportDto } from 'src/app/dto'

export class DeviceService {
  async create(prisma: PrismaClient, dto: CreateDeviceReportDto) {
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

    return report
  }
}
