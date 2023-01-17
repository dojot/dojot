const path = require('path');

const {
  Logger, ConfigManager, WebUtils: { SecretFileHandler },
} = require('@dojot/microservice-sdk');
const App = require('./app/app');

// External dependencies
const openApiPath = path.join(__dirname, '../docs/v1.yml');
ConfigManager.loadSettings('FILEMGMT', 'default.conf');
const config = ConfigManager.getConfig('FILEMGMT');

const logger = new Logger('file-mgmt:Server');
Logger.setTransport('console', {
  level: config.logger['console.level'],
});
Logger.setVerbose(config.logger.verbose);

logger.debug('Loading secrets');
Logger.setTransport('console', {
  level: config.log['console.level'],
});
Logger.setVerbose(config.log.verbose);

const secretsLoader = new SecretFileHandler(config, logger);
secretsLoader.handleCollection(['minio.access.key', 'minio.secret.key', 'keycloak.client.secret'], '/secrets/').then(() => {
  // Init Application
  const app = new App(config, logger, openApiPath);

  app.init().then(() => {
    logger.info('Server started..');
  }).catch((error) => {
    logger.error(error);
    process.kill(process.pid, 'SIGTERM');
  });
}).catch((error) => {
  logger.error(error);
  process.kill(process.pid, 'SIGTERM');
});

process.on('unhandledRejection', async (reason) => {
  // The 'unhandledRejection' event is emitted whenever a Promise is rejected and
  // no error handler is attached to the promise within a turn of the event loop.
  logger.error(`Unhandled Rejection at: ${reason.stack || reason}.`);

  process.kill(process.pid, 'SIGTERM');
});

// eslint-disable-next-line security-node/detect-improper-exception-handling
process.on('uncaughtException', async (ex) => {
  // The 'uncaughtException' event is emitted when an uncaught JavaScript
  logger.error(`uncaughtException: Unhandled Exception at: ${ex.stack || ex}. Bailing out!!`);

  process.kill(process.pid, 'SIGTERM');
});
