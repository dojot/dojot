const {
  ConfigManager,
  Logger,
  WebUtils: {
    SecretFileHandler,
  },
} = require('@dojot/microservice-sdk');

const util = require('util');

// Creating the configuration
const userConfigFile = process.env.RETRIEVER_USER_CONFIG_FILE || 'production.conf';
ConfigManager.loadSettings('RETRIEVER', userConfigFile);
const config = ConfigManager.getConfig('RETRIEVER');

const {
  log: configLog,
} = config;
Logger.setTransport('console', {
  level: configLog['console.level'],
});
Logger.setVerbose(configLog.verbose);
if (configLog.file) {
  Logger.setTransport('file', {
    level: configLog['file.level'],
    filename: configLog['file.filename'],
  });
}

const logger = new Logger('influxdb-retriever');

logger.info(`The current configuration is:\n${util.inspect(config, false, 5, true)}`);

const App = require('./app/App');
const dependeciesContainerFactory = require('./app/DependencyContainer');

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

// Initializing the service...
const secretFileHandler = new SecretFileHandler(config, logger);

secretFileHandler.handle('keycloak.client.secret', '/secrets/').then(async () => {
  try {
    logger.info('Initializing...');
    const dependencyContainer = dependeciesContainerFactory(config, logger);
    const app = new App(dependencyContainer, logger, config.influx);
    await app.init();
  } catch (err) {
    logger.error('Service will be closed', err);
    process.kill(process.pid, 'SIGTERM');
  }
}).catch(() => {
  logger.debug('Obtaining the secrets failed!');
});
