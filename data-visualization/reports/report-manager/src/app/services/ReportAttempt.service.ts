import { PrismaClient } from '@prisma/client'

import { CreateReportAttemptDto, UpdateReportAttemptDto } from 'src/app/dto'

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
