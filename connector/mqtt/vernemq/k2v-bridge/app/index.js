/**
 * @override Initialize a k2v-client creating an Agent Messenger
 * then initialize it, also set log level for the service.
 * @author Dojot
 */

const { logger } = require('@dojot/dojot-module-logger');
const config = require('./config');
const MqttClient = require('./MqttClient');

const mqttClient = new MqttClient();

/* set log level */
logger.setLevel(config.app.logLevel);

mqttClient.init();
