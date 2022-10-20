import { Express } from 'express';
import { Logger, ServiceStateManager, WebUtils } from '@dojot/microservice-sdk';

import { PrismaClientInterceptor } from './interceptors';
import { AppConfig } from '../types';
import { KafkaConsumer, TenantManager, KafkaProducer } from '../kafka';
import { DeviceRoutes, TemplateRoutes } from '../app/routes';
import { createHttpTerminator } from 'http-terminator';
import { Server } from 'http';

export class App {
  constructor(
    private logger: Logger,
    private appconfig: AppConfig,
    private kafkaConsumer: KafkaConsumer,
    private tenantManager: TenantManager,
    private KafkaProducer: KafkaProducer,
    private serviceState: ServiceStateManager,
  ) {}

  private createServer(): Server {
    return WebUtils.createServer(this.logger, this.appconfig);
  }

  private createExpress(): Express {
    const { createKeycloakAuthInterceptor, jsonBodyParsingInterceptor } =
      WebUtils.framework.interceptors;

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
            limit: this.appconfig.express['parsing.limit'],
          },
        }),
        createKeycloakAuthInterceptor(
          this.tenantManager.tenants,
          this.logger,
          '/',
        ),
        PrismaClientInterceptor.use(this.logger, this.appconfig),
      ],
      routes: [
        DeviceRoutes.use(this.logger, this.KafkaProducer),
        TemplateRoutes.use(this.logger),
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

    this.kafkaConsumer.initNewTenantEvent(
      this.tenantManager.create.bind(this.tenantManager),
    );
    await this.KafkaProducer.init();

    const server = this.createServer();
    const framework = this.createExpress();

    server.on('request', framework);

    server.on('listening', this.onListening);
    server.on('close', () => {
      this.serviceState.signalNotReady('dojot-device-manager-batch');
    });

    server.on('error', () => {
      this.logger.info('Received error event', {});
      this.serviceState.signalNotReady('dojot-device-manager-batch');
    });

    // shutdown
    createHttpTerminator({ server: server });

    return server.listen(this.appconfig.api.port, () => {
      this.logger.info(`Server running at port ${this.appconfig.api.port}`, {});
    });
  }
}
