const { ConfigManager, Logger } = require('@dojot/microservice-sdk');

const util = require('util');

const MQTTClient = require('./app/MqttClient');
const utils = require('./app/utils');

/*
 * TODO: the idea is to create a default environment variable that will be used internally by the
 * ConfigManager to automatically select the user config file. Since this is not something we care
 * to much now, this will do the trick.
 */
const userConfigFile = process.env.K2V_APP_USER_CONFIG_FILE || 'production.conf';
ConfigManager.loadSettings('K2V', userConfigFile);
const config = ConfigManager.getConfig('K2V');

// Logger configuration
Logger.setVerbose(config.log.verbose);
Logger.setTransport('console', { level: config.log['console.level'] });
if (config.log['file.enable']) {
  Logger.setTransport('file', {
    level: config.log['file.level'],
    filename: config.log['file.filename'],
  });
}
const logger = new Logger('k2v:index');

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
  utils.killApplication();
});

const mqttClient = new MQTTClient();

try {
  mqttClient.init();
} catch (error) {
  logger.error('An error occurred while initializing the MQTT Client. Bailing out!');
  logger.error(error.stack || error);
  utils.killApplication();
}
