
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
  constructor(
    private logger: Logger,
    private config: AppConfig,
    private kafkaConsumer: KafkaConsumer,
    private tenantManager: TenantManager,
    private KafkaProducer: KafkaProducer
  ) 
  {}

  private createExpress(): Express {
    const { createKeycloakAuthInterceptor, jsonBodyParsingInterceptor } =
      WebUtils.framework.interceptors

      return WebUtils.framework.createExpress({
        server: undefined,
        logger: this.logger,
        supportWebsockets: false,
        supportTrustProxy: false,
        catchInvalidRequest: false,
        errorHandlers: undefined as unknown as unknown[],
        interceptors: [
          jsonBodyParsingInterceptor({
            config: {
              limit: this.config.express['parsing.limit'],
            },
          }),
          createKeycloakAuthInterceptor(
            this.tenantManager.tenants,
            this.logger,
            '/',
          ),
          PrismaClientInterceptor.use(this.logger,this.config),
        ],
        routes: [
          DeviceRoutes.use(this.logger), 
          TemplateRoutes.use(this.logger)].flat(),
      })
    }


    async init() {
    
    await this.kafkaConsumer.init()
    await this.tenantManager.update()
  
    this.kafkaConsumer.initNewTenantEvent(
        this.tenantManager.create.bind(this.tenantManager),
      )
      
    await this.KafkaProducer.init()
  
     const express = this.createExpress()
     
     return express.listen(this.config.api.port, () => {
      this.logger.info(`Server running at port ${this.config.api.port}`, {})
    })
    }
}