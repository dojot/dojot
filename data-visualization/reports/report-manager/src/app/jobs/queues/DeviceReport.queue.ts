import { Report } from '@prisma/client'
import { Logger } from '@dojot/microservice-sdk'
// eslint-disable-next-line import/no-named-as-default
import Queue, { Queue as QueueType } from 'bull'

import { Config } from 'src/types'

import { DeviceReportProcessor } from '../processors/DeviceReport.processor'

type CreateReportParams = {
  lang: string
  tenant: string
}

export class DeviceReportQueue {
  private queue: QueueType

  constructor(
    private logger: Logger,
    private config: Config,
    private deviceReportProcessor: DeviceReportProcessor,
  ) {
    this.queue = new Queue('DeviceReport', {
      redis: {
        db: this.config.redis.db,
        host: this.config.redis.host,
        port: this.config.redis.port,
      },
    })

    this.queue.process(
      this.deviceReportProcessor.process.bind(this.deviceReportProcessor),
    )

    this.logger.debug('constructor: device report queue instantiated', {})
  }

  async createReport(report: Report, { lang, tenant }: CreateReportParams) {
    this.logger.debug('createReport: device report added to the queue', {
      lang,
      tenant,
    })

    return this.queue.add({ lang, tenant, report })
  }
}
