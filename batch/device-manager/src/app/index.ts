import { Express } from 'express';
import { Logger, ServiceStateManager, WebUtils } from '@dojot/microservice-sdk';
import { PrismaUtils } from 'src/utils/Prisma.utils';

import { AppConfig } from '../types';
import { KafkaConsumer, TenantManager, KafkaProducer } from '../kafka';
import { DeviceRoutes } from '../app/routes';

import {
  DefaultErrorHandlerInterceptor,
  KafkaProducerClientInterceptor,
  PrismaClientInterceptor,
} from './interceptors';
import { TemplateRoutes } from './routes/Template.routes';

export class App {
  constructor(
    private logger: Logger,
    private appconfig: AppConfig,
    private prismaUtils: PrismaUtils,
    private kafkaConsumer: KafkaConsumer,
    private tenantManager: TenantManager,
    private KafkaProducer: KafkaProducer,
    private serviceState: ServiceStateManager,
  ) {}

  private createExpress(): Express {
    const { createKeycloakAuthInterceptor, jsonBodyParsingInterceptor } =
      WebUtils.framework.interceptors;

    return WebUtils.framework.createExpress({
      server: undefined,
      logger: this.logger,
      supportWebsockets: false,
      supportTrustProxy: false,
      catchInvalidRequest: false,
      errorHandlers: [
        DefaultErrorHandlerInterceptor.use(this.prismaUtils),
        WebUtils.framework.defaultErrorHandler({
          logger: this.logger,
        }),
      ],
      interceptors: [
        jsonBodyParsingInterceptor({
          config: {
            limit: this.appconfig.express['parsing.limit'],
          },
        }),
        createKeycloakAuthInterceptor(
          this.tenantManager.tenants,
          this.logger,
          '/',
        ),
        KafkaProducerClientInterceptor.use(
          this.logger,
          this.appconfig,
          this.KafkaProducer,
        ),
        PrismaClientInterceptor.use(
          this.logger,
          this.appconfig,
          this.prismaUtils,
        ),
      ],
      routes: [
        DeviceRoutes.use(this.logger, this.KafkaProducer, this.prismaUtils),
        TemplateRoutes.use(this.logger, this.KafkaProducer, this.prismaUtils),
      ].flat(),
    });
  }

  onListening = () => {
    this.logger.info('Server ready to accept connections!', {});
    //this.logger.info(this.server.address());
    this.serviceState.signalReady('dojot-device-manager-batch');
  };

  async init() {
    await this.kafkaConsumer.init();
    await this.tenantManager.update();

    await this.KafkaProducer.init();

    const express = this.createExpress();
    const server = express.listen(this.appconfig.api.port, () => {
      this.logger.info(`Server running at port ${this.appconfig.api.port}`, {});
    });

    server.on('listening', this.onListening);
    server.on('close', () => {
      this.serviceState.signalNotReady('dojot-device-manager-batch');
    });

    server.on('error', () => {
      this.logger.info('Received error event', {});
      this.serviceState.signalNotReady('dojot-device-manager-batch');
    });

    return server;
  }
}
