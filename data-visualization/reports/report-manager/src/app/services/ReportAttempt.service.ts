import { PrismaClient } from '@prisma/client'

import { CreateReportAttemptDto, UpdateReportAttemptDto } from 'src/app/dto'

/**
 * This service is very simple, but can become complex in the future.
 * That's why I called it "service" and not a "repository"
 */

export class ReportAttemptService {
  async create(prisma: PrismaClient, dto: CreateReportAttemptDto) {
    return prisma.reportAttempt.create({
      data: { reportId: dto.reportId },
    })
  }

  async update(prisma: PrismaClient, id: string, dto: UpdateReportAttemptDto) {
    return prisma.reportAttempt.update({
      where: { id },
      data: dto,
    })
  }
}
