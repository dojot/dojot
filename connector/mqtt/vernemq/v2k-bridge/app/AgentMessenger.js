const { ConfigManager, Kafka: { Producer }, Logger } = require('@dojot/microservice-sdk');

const Utils = require('./utils');
const MQTTClient = require('./MQTTClient');

/**
 * Class representing an AgentMessenger
 *
 * @class
 */
class AgentMessenger {
  /**
   * Create an agentMessenger
   */
  constructor() {
    this.config = ConfigManager.getConfig('V2K');

    this.initialized = false;
    this.producer = new Producer({
      ...this.config.sdk,
      'kafka.producer': this.config.producer,
      'kafka.topic': this.config.topic,
    });
    this.mqttClient = new MQTTClient(this);
    this.logger = new Logger('AgentMessenger');
  }

  /**
   * Initialize the agent messenger on success init mqttClient on error exit.
   *
   * @function init
   */
  init() {
    this.logger.info('Initializing Kafka Producer...');
    this.producer.connect().then(() => {
      this.logger.info('... Kafka Producer was initialized');

      // initializing mqtt client
      this.logger.info('Initializing MQTTClient');
      this.mqttClient.init();
    }).catch((error) => {
      this.logger.error(error.stack || error);
      process.exit(1);
    });
  }

  /**
   * Produce a given message to a given topic.
   *
   * @function sendMessage
   *
   * @param {string} topic
   * @param {Object} message
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
      kafkaTopic = `${deviceDataMessage.metadata.tenant}.${
        this.config.messenger['produce.topic.suffix']}`;
      deviceDataMessage = JSON.stringify(deviceDataMessage);
    } catch (error) {
      this.logger.error(`Failed to create the message. Error: ${error.stack || error}`);
      return;
    }

    this.logger.debug(`Trying to send message to kafka topic ${kafkaTopic}...`);
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
