const {
  ConfigManager,
  Logger,
  WebUtils: {
    SecretFileHandler,
  },
} = require('@dojot/microservice-sdk');
const util = require('util');

const { killApplication } = require('./app/Utils');

// Creating the configuration
const userConfigFile =
  process.env.BASIC_AUTH_USER_CONFIG_FILE || 'production.conf';
ConfigManager.loadSettings('BASIC_AUTH', userConfigFile);
const config = ConfigManager.getConfig('BASIC_AUTH');

const { log: configLog } = config;
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

const logger = new Logger('basic-auth:index');

logger.info(
  `The current configuration is:\n${util.inspect(config, false, 5, true)}`,
);

const App = require('./app/App');

process.on('unhandledRejection', async (reason) => {
  // The 'unhandledRejection' event is emitted whenever a Promise is rejected and
  // no error handler is attached to the promise within a turn of the event loop.
  logger.error(`Unhandled Rejection at: ${reason.stack || reason}.`);

  killApplication();
});

process.on('uncaughtException', async (ex) => {
  // The 'uncaughtException' event is emitted when an uncaught JavaScript
  // exception bubbles all the way back to the event loop.
  logger.error(
    `uncaughtException: Unhandled Exception at: ${
      ex.stack || ex
    }. Bailing out!!`,
  );

  killApplication();
});

const secretFileHandler = new SecretFileHandler(config, logger);
// Initializing the service...
secretFileHandler.handle('keycloak.client.secret', '/secrets/').then(async () => {
  setTimeout(async () => {
    try {
      logger.info('Initializing...');
      const app = new App(config);
      await app.init();
    } catch (err) {
      logger.error('Service will be closed', err);
      killApplication();
    }
  }, 15000);
});
