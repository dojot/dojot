import { Logger } from '@dojot/microservice-sdk'
import { NextFunction, Request, Response } from 'express'

import { ReportService } from 'src/app/services'

export class ReportController {
  constructor(private logger: Logger, private reportService: ReportService) {}

  async findMany(req: Request, res: Response, next: NextFunction) {
    try {
      this.logger.debug('findMany: finding reports', { query: req.query })

      const name = req.query.name ? String(req.query.name) : undefined
      const page = req.query.page ? Number(req.query.page) : undefined
      const pageSize = req.query.pageSize
        ? Number(req.query.pageSize)
        : undefined

      const { reports, total } = await this.reportService.findMany(req.prisma, {
        name,
        page,
        pageSize,
      })

      res.status(200).json({
        reports,
        pagination: {
          total,
          page,
          pageSize,
        },
      })

      this.logger.debug(`findMany: ${reports.length}/${total} reports found`, {
        query: req.query,
      })

      return next()
    } catch (e) {
      next(e)
    }
  }

  async findById(req: Request, res: Response, next: NextFunction) {
    try {
      this.logger.debug('findById: finding report by id', { id: req.params.id })

      const report = await this.reportService.findById(
        req.prisma,
        req.params.id,
      )

      if (report) {
        res.status(200).json(report)
        this.logger.debug('findById: report found', { id: req.params.id })
      } else {
        res.status(404).json({ message: 'Report not found' })
        this.logger.debug('findById: report not found', { id: req.params.id })
      }

      return next()
    } catch (e) {
      next(e)
    }
  }

  async delete(req: Request, res: Response, next: NextFunction) {
    try {
      this.logger.debug('delete: deleting report by id', { id: req.params.id })

      const deletedReport = await this.reportService.delete(
        req.prisma,
        req.params.id,
        req.headers.authorization || '',
      )

      if (deletedReport) {
        res.status(200).json(deletedReport)
        this.logger.debug('delete: report deleted', { id: req.params.id })
      } else {
        res.status(404).json({ message: 'Report not found' })
        this.logger.debug('delete: report not found', { id: req.params.id })
      }

      return next()
    } catch (e) {
      next(e)
    }
  }
}
