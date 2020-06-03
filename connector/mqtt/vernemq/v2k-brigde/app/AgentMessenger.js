const { Kafka: { Producer }, Logger } = require('@dojot/microservice-sdk');
const { messenger, producer: sdkConfig, mqtt: mqttConfig } = require('./config');
const Utils = require('./utils');
const MQTTClient = require('./MqttClient');

/**
 * Class representing an AgentMessenger
 *
 * @class
 */
class AgentMessenger {
  /**
   * Create an agentMessenger
   *
   * @param {Object} config - the configuration
   */
  constructor(config) {
    this.initialized = false;
    this.producer = new Producer(sdkConfig);
    this.mqttClient = new MQTTClient(this, config || mqttConfig);
    this.logger = new Logger('AgentMessenger');
  }

  /**
   * Initialize the agent messenger on success init mqttClient on error exit.
   *
   * @function init
   */
  init() {
    this.logger.debug('Initializing Kafka Producer...');
    this.producer.connect().then(() => {
      this.logger.debug('... Kafka Producer was initialized');

      // initializing mqtt client
      this.logger.debug('Initializing MQTTClient');
      this.mqttClient.init();
    }).catch(() => {
      this.logger.error('An error occurred while initializing the Kafka Producer. Bailing out!');
      process.exit(1);
    });
  }

  /**
   * Produce a given message to a given topic.
   *
   * @function sendMessage
   *
   * @param {string} topic - topic to produce
   * @param {Object} message - message to produce
   */
  sendMessage(topic, message) {
    let jsonPayload;
    let deviceDataMessage;
    let messageKey;
    let kafkaTopic;

    try {
      jsonPayload = JSON.parse(message);
      deviceDataMessage = Utils.generateDojotDeviceDataMessage(topic, jsonPayload);
      messageKey = `${deviceDataMessage.metadata.tenant}:${deviceDataMessage.metadata.deviceid}`;
      kafkaTopic = `${deviceDataMessage.metadata.tenant}.${messenger.kafkaTopic}`;
      deviceDataMessage = JSON.stringify(deviceDataMessage);
    } catch (error) {
      this.logger.error(`Failed to create the message. Error: ${error}`);
      return;
    }

    this.logger.debug('Sending message to kafka');
    this.producer.produce(kafkaTopic, deviceDataMessage, messageKey).then(() => {
      this.logger.debug(`Successfully sent message to Kafka in ${kafkaTopic}`);
    }).catch((error) => {
      this.logger.error(`Error while sending message to Kafka in ${kafkaTopic}`);
      if (error) {
        this.logger.error(error.stack || error);
      }
    });
  }
}

module.exports = AgentMessenger;
