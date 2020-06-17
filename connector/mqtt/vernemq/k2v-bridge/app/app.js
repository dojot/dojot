const { Logger } = require('@dojot/microservice-sdk');

const util = require('util');
const { unflatten } = require('flat');

const config = require('./config');
const MQTTClient = require('./MqttClient');

// Logger configuration
const logConfig = unflatten(config.logger);
Logger.setTransport('console', logConfig.transports.console);
Logger.setVerbose(logConfig.verbose);
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

const mqttClient = new MQTTClient();

try {
  mqttClient.init();
} catch (error) {
  logger.error('An error occurred while initializing the MQTT Client. Bailing out!');
  logger.error(error.stack || error);
  process.exit(1);
}
