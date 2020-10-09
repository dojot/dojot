const {
  ConfigManager,
  Logger,
} = require('@dojot/microservice-sdk');

const util = require('util');


// Creating the configuration
const userConfigFile = process.env.CERT_SC_USER_CONFIG_FILE || 'production.conf';
ConfigManager.loadSettings('CERT_SC', userConfigFile);
const config = ConfigManager.getConfig('CERT_SC');

// Logger configuration
const {
  log: configLog, app: configApp,
} = config;
Logger.setTransport('console', {
  level: configLog['console.level'],
});
Logger.setVerbose(configLog['console.verbose']);
if (configLog.file) {
  Logger.setTransport('file', {
    level: configLog['file.level'],
    filename: configLog['file.filename'],
  });
}
const logger = new Logger(`cert-sc-${configApp['sidecar.to']}:index`);

logger.info(`The final configuration:\n${util.inspect(config, false, 5, true)}`);

const App = require('./app/App');
const ServiceStateMgmt = require('./app/ServiceStateMgmt');

const unhandledRejections = new Map();
// the unhandledRejections Map will grow and shrink over time,
// reflecting rejections that start unhandled and then become handled.
process.on('unhandledRejection', async (reason, promise) => {
  // The 'unhandledRejection' event is emitted whenever a Promise is rejected and
  // no error handler is attached to the promise within a turn of the event loop.
  logger.error(`Unhandled Rejection at: ${reason.stack || reason}.`);

  unhandledRejections.set(promise, reason);

  logger.debug(`unhandledRejection: List of Unhandled Rejection size ${unhandledRejections.size}`);
});

process.on('rejectionHandled', (promise) => {
  // The 'rejectionHandled' event is emitted whenever a Promise has
  // been rejected and an error handler was attached to it
  // later than one turn of the Node.js event loop.
  logger.debug('rejectionHandled: A event');

  unhandledRejections.delete(promise);

  logger.debug(`rejectionHandled: List of Unhandled Rejection size ${unhandledRejections.size}`);
});

process.on('uncaughtException', async (ex) => {
  // The 'uncaughtException' event is emitted when an uncaught JavaScript
  // exception bubbles all the way back to the event loop.
  logger.error(`uncaughtException: Unhandled Exception at: ${ex.stack || ex}. Bailing out!!`);

  await ServiceStateMgmt.shutdown();
});

const app = new App();

// Initializing the service...
(async () => {
  try {
    logger.info('Initializing...');
    await app.init();
  } catch (err) {
    logger.error('Service will be close', err);
    try {
      await ServiceStateMgmt.shutdown();
    } catch (e) {
      logger.error('Shutdown:', e);
      process.kill(process.pid, 'SIGTERM');
    }
  }
})();
