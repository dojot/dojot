import App from './app'
import dotenv from 'dotenv';
import { Logger, ConfigManager } from '@dojot/microservice-sdk';
import KafkaConsumer from './kafka/consumer/kafka-consumer';
import TenantManagerService from './services/tenantManagerService';

ConfigManager.loadSettings('DEVICEMANAGERBATCH', 'default.conf', './config', './src');
const config = ConfigManager.getConfig('DEVICEMANAGERBATCH', './config', './src');
const logger = new Logger('device-manager-batch');
Logger.setLevel('console', 'debug')

const kafkaConsumer = new KafkaConsumer(logger, config);
const tenantManagerService = new TenantManagerService(logger);

const app = new App(logger, config, kafkaConsumer, tenantManagerService);
app.init().then(() => {
  const server = app.express.listen(config.api.port, () => {
    console.log(`⚡️[server]:  Server Batch is running at http://localhost:${config.api.port}`);
  });

  ['SIGTERM', 'SIGINT'].forEach((sig) => process.on(sig, () => {
    server.close(() => {
      process.exit(0);
    });
  }));
})

process.on('unhandledRejection', async (ex: any) => {
  // The 'unhandledRejection' event is emitted whenever a Promise is rejected and
  // no error handler is attached to the promise within a turn of the event loop.
  logger.error(`Unhandled Rejection at: ${ex.stack || ex}.`, {});
  process.kill(process.pid, 'SIGTERM');
});


process.on('uncaughtException', async (ex: any) => {
  // The 'uncaughtException' event is emitted when an uncaught JavaScript
  // exception bubbles all the way back to the event loop.
  logger.error(`uncaughtException: Unhandled Exception at: ${ex.stack || ex}. Bailing out!!`, {});
  process.kill(process.pid, 'SIGTERM');
});
