import { Logger } from '@dojot/microservice-sdk'
import { NextFunction, Request, Response } from 'express'

import { DeviceService } from 'src/app/services'

export class DeviceController {
  constructor(private logger: Logger, private deviceService: DeviceService) {}

  async create(req: Request, res: Response, next: NextFunction) {
    try {
      this.logger.debug('create: creating device report', {})

      const report = await this.deviceService.create(req.prisma, req.body, {
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
