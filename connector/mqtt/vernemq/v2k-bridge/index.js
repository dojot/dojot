const { ConfigManager, Logger } = require('@dojot/microservice-sdk');

const util = require('util');

const AgentMessenger = require('./app/AgentMessenger');

// ABCDEF

/*
 * TODO: the idea is to create a default environment variable that will be used internally by the
 * ConfigManager to automatically select the user config file. Since this is not something we care
 * to much now, this will do the trick.
 */
const userConfigFile = process.env.V2K_APP_USER_CONFIG_FILE || 'production.conf';
// Creating the configuration
ConfigManager.loadSettings('V2K', userConfigFile);
const config = ConfigManager.getConfig('V2K');

// Logger configuration
Logger.setTransport('console', { level: config.logger['transports.console.level'] });
Logger.setVerbose(config.logger.verbose);
const logger = new Logger('app');

logger.info(`Configuration:\n${util.inspect(config, false, 5, true)}`);

const unhandledRejections = new Map();
// the unhandledRejections Map will grow and shrink over time,
// reflecting rejections that start unhandled and then become handled.
process.on('unhandledRejection', (reason, promise) => {
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
  // TODO: stop server (connection redis, kafka consumer, etc.)
  process.exit(1);
});

const agentMessenger = new AgentMessenger();

try {
  agentMessenger.init();
} catch (error) {
  logger.error('An error occurred while initializing the Agent Messenger. Bailing out!');
  logger.error(error.stack || error);
  process.exit(1);
}
