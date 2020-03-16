/**
 * @override Initializes a k2v-bridge creating 'an Agent Messenger
 * then initializes it, also set log level for the service.
 */

const { logger } = require('@dojot/dojot-module-logger');
const config = require('./config');
const MqttClient = require('./MqttClient');

const mqttClient = new MqttClient();

/* set log level */
logger.setLevel(config.app.logLevel);

mqttClient.init();
