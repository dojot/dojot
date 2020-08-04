/**
 * A simple MQTT example to show how to use the ConfigManager.
 */
/* eslint-disable no-console */
const { ConfigManager, Logger } = require('@dojot/microservice-sdk');
const camelCase = require('lodash/camelCase');
const mqtt = require('mqtt');

Logger.setTransport('console');
const logger = new Logger('config-example');

// Setting up the configuration
ConfigManager.loadSettings('CONFEXAMPLE');
const config = ConfigManager.getConfig('CONFEXAMPLE');
// The MQTT library configuration is all in camelCase format, so we need to convert it before using
const mqttConfig = ConfigManager.transformObjectKeys(config.mqtt, camelCase);

logger.info(config);
logger.info('Tranformed MQTT config:');
logger.info(mqttConfig);

logger.info('Waiting VerneMQ to be connected...');

logger.info('Connecting to the broker...');
const client = mqtt.connect(mqttConfig.hostname, mqttConfig);

client.on('connect', () => {
  logger.info('... Connected to the Broker');
  let counter = 0;

  client.subscribe(config.client['topics.subscribe'], (err) => {
    if (err) {
      logger.error(err.stack || err);
      process.exit(1);
    }

    logger.info(`Subscribed to the topic ${config.client['topics.subscribe']}`);
    setInterval(() => {
      client.publish(config.client['topics.publish'], `hello ${counter}`);
      counter += 1;
    }, config.client['message.pub.interval']);
  });
});

client.on('message', (topic, payload) => {
  logger.info(`Received message ${payload} in topic ${topic}`);
});
