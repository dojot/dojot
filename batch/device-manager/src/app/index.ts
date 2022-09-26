
import { Express } from 'express'
import { Logger, WebUtils } from '@dojot/microservice-sdk'

import {
  ErrorHandlerInterceptor,
  PrismaClientInterceptor,
} from './interceptors'
import { AppConfig } from '../types'
import { KafkaConsumer,TenantManager } from '../kafka'
import { DeviceRoutes, TemplateRoutes } from '../app/routes'

export class App {
  private express: Express

  constructor(
    private logger: Logger,
    private config: AppConfig,
    private kafkaConsumer: KafkaConsumer,
    private tenantManager: TenantManager,
  ) {
    const { jsonBodyParsingInterceptor,createKeycloakAuthInterceptor } =
      WebUtils.framework.interceptors

    this.express = WebUtils.framework.createExpress({
      logger,
      server: undefined,
      supportWebsockets: false,
      supportTrustProxy: false,
      catchInvalidRequest: false,
      interceptors: [
        jsonBodyParsingInterceptor({
          config: {
            limit: config.express['parsing.limit'],
          },
        }),
        createKeycloakAuthInterceptor(this.tenantManager.tenants,
          this.logger,
          '/',),
        PrismaClientInterceptor.use(),
      ],
      errorHandlers: [ErrorHandlerInterceptor.use()],
      routes: [DeviceRoutes.use(logger), TemplateRoutes.use(logger)].flat(),
    })
  }

  async init() {
    await this.kafkaConsumer.init()

    this.kafkaConsumer.initNewTenantEvent(
      this.tenantManager.create.bind(this.tenantManager),
    )

    return this.express.listen(this.config.api.port, () => {
      this.logger.info(`Server running at port ${this.config.api.port}`, {})
    })
  }
}