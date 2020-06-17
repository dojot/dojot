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



const mqttClient = new MQTTClient();

try {
  mqttClient.init();
} catch (error) {
  logger.error('An error occurred while initializing the MQTT Client. Bailing out!');
  logger.error(error.stack || error);
  process.exit(1);
}
