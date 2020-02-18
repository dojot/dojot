const { logger } = require('@dojot/dojot-module-logger');

const config = require('./config');
const MQTTClient = require('./MqttClient');

/* set log level */
logger.setLevel(config.app.mqttLogLevel);

const client = new MQTTClient(config);
client.init();
