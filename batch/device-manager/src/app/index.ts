
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
      
    const keycloakClientSession = new WebUtils.KeycloakClientSession(
        process.env.KEYCLOAK_URI || '',
        process.env.KEYCLOAK_REALM || '',
        {
          grant_type: 'client_credentials',
          client_id: process.env.KEYCLOAK_CLIENT_ID || '',
          client_secret: process.env.KEYCLOAK_CLIENT_SECRET || '',
        },
        this.logger,
        { retryDelay: 5000 },
      )

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


  return this.express.listen(this.config.api.port, () => {
      this.logger.info(`Server running at port ${this.config.api.port}`, {})
    })
  }
}