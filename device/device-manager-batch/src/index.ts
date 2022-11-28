import {
  ConfigManager,
  Logger,
  ServiceStateManager,
  WebUtils,
} from '@dojot/microservice-sdk';
import camelCase from 'lodash.camelcase';

import { AppConfig } from 'src/types';

import { KafkaConsumer, TenantManager, KafkaProducer } from './kafka';
import { App } from './app';
import { PrismaUtils } from './utils/Prisma.utils';

ConfigManager.loadSettings(
  'DEVICE_MANAGER_BATCH',
  'default.conf',
  './config',
  './build',
);

const config: AppConfig = ConfigManager.getConfig(
  'DEVICE_MANAGER_BATCH',
  './config',
  './build',
);

const logger = new Logger('dojot-device-manager-batch');
Logger.setLevel('console', 'debug');

const dojotHttpClient = new WebUtils.DojotHttpClient({
  logger,
  defaultClientOptions: {},
  defaultRetryDelay: 15000,
  defaultMaxNumberAttempts: 0,
});
const prismaUtils = new PrismaUtils(logger, config);
const tenantManager = new TenantManager(
  logger,
  config,
  dojotHttpClient,
  prismaUtils,
);
const kafkaConsumer = new KafkaConsumer(logger, config, tenantManager);
const kafkaProducer = new KafkaProducer(logger, config);
const secretFileHandler = new WebUtils.SecretFileHandler(config, logger);
const serviceState = new ServiceStateManager({
  lightship: ConfigManager.transformObjectKeys(
    config.lightship['detect.kubernetes'],
    camelCase,
  ),
});

serviceState.registerService('dojot-device-manager-batch');

secretFileHandler
  .handle('keycloak.client.secret', '/secrets/')
  .then(async () => {
    try {
      logger.info('Starting application', {});

      const app = new App(
        logger,
        config,
        prismaUtils,
        kafkaConsumer,
        tenantManager,
        kafkaProducer,
        serviceState,
      );

      const server_app = await app.init();

      const handleCloseServerAndExitProcess = () => {
        server_app.close(() => process.exit(0));
      };

      process.on('SIGINT', handleCloseServerAndExitProcess);
      process.on('SIGTERM', handleCloseServerAndExitProcess);

      process.on('unhandledRejection', (e: Error) => {
        logger.error(`Unhandled rejection: ${e.stack || e}`, {});
        process.kill(process.pid, 'SIGTERM');
      });

      process.on('uncaughtException', (e: Error) => {
        logger.error(
          `uncaughtException: Unhandled exception: ${e.stack || e}`,
          {},
        );
        process.kill(process.pid, 'SIGTERM');
      });

      logger.info('Application Device Manager Batch is running', {});
    } catch (error: unknown) {
      logger.error('Application will be closed: ', { error });
      process.kill(process.pid, 'SIGTERM');
    }
  });
