import { PrismaClient } from '@prisma/client'

import { CreateReportFileDto } from 'src/app/dto'

export class ReportFileService {
  async create(prisma: PrismaClient, dto: CreateReportFileDto) {
    return prisma.reportFile.create({
      data: dto,
    })
  }
}
