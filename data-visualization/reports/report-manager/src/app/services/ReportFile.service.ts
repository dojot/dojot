import { PrismaClient } from '@prisma/client'

import { CreateReportFileDto } from 'src/app/dto'

/**
 * This service is very simple, but can become complex in the future.
 * That's why I called it "service" and not a "repository"
 */

export class ReportFileService {
  async create(prisma: PrismaClient, dto: CreateReportFileDto) {
    return prisma.reportFile.create({
      data: dto,
    })
  }
}
