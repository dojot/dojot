
import { Express } from 'express'
import { Logger, WebUtils } from '@dojot/microservice-sdk'

import {
  ErrorHandlerInterceptor,
  PrismaClientInterceptor,
  ErrorKeycloakHandlerInterceptor
} from './interceptors'
import { AppConfig } from '../types'
import { KafkaConsumer,TenantManager } from '../kafka'
import { DeviceRoutes, TemplateRoutes } from '../app/routes'
import KafkaProducer from 'src/kafka/kafka-producer'

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
        PrismaClientInterceptor.use(logger,config),
      ],
      errorHandlers: [ErrorHandlerInterceptor.use(),ErrorKeycloakHandlerInterceptor.use()],
      routes: [DeviceRoutes.use(logger), TemplateRoutes.use(logger)].flat(),
    })
  }

  async init() {
    
    await this.kafkaConsumer.init()
    await this.tenantManager.update()

    this.kafkaConsumer.initNewTenantEvent(
      this.tenantManager.create.bind(this.tenantManager),
    )
    
    //await this.kafkaProducer.init()


  return this.express.listen(this.config.api.port, () => {
      this.logger.info(`Server Device Manager Batch running at port ${this.config.api.port}`, {})
    })
  }
}