import { Logger } from '@dojot/microservice-sdk'
import { NextFunction, Request, Response } from 'express'

import { DeviceService } from 'src/app/services'
import { DeviceReportQueue } from 'src/app/jobs'
import { CreateDeviceReportDto } from 'src/app/dto'

export class DeviceController {
  constructor(
    private logger: Logger,
    private deviceService: DeviceService,
    private deviceReportQueue: DeviceReportQueue,
  ) {}

  async create(req: Request, res: Response, next: NextFunction) {
    try {
      this.logger.debug('create: creating device report', {})

      const dto = req.body as CreateDeviceReportDto
      const report = await this.deviceService.create(req.prisma, dto)

      await this.deviceReportQueue.createReport(report, {
        lang: req.lang,
        tenant: req.tenant.id,
      })

      res.status(200).json({ id: report.id })

      this.logger.debug('create: report record created', {
        reportId: report.id,
      })

      return next()
    } catch (e) {
      next(e)
    }
  }
}
