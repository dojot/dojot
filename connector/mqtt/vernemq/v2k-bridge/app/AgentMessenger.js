const { ConfigManager, Kafka: { Producer }, Logger } = require('@dojot/microservice-sdk');

const Utils = require('./Utils');

/**
 * Class representing an AgentMessenger
 *
 * @class
 */
class AgentMessenger {
  /**
   * Create an agentMessenger
   *
   * @constructor
   */
  constructor() {
    this.config = ConfigManager.getConfig('V2K');

    this.producer = new Producer({
      ...this.config.sdk,
      'kafka.producer': this.config.producer,
      'kafka.topic': this.config.topic,
    });
    this.logger = new Logger('v2k:agent-messenger');
  }

  /**
   * Initialize the Kafka Producer. It initializes the MQTT connection after a successful
   * connection.
   *
   * @param {Object} mqttClient
   *
   * @function init
   * @public
   */
  init(mqttClient) {
    this.logger.info('Initializing Kafka Producer...');
    this.producer.connect().then(() => {
      this.logger.info('... Kafka Producer was initialized');

      // initializing mqtt client
      this.logger.info('Initializing MQTTClient');
      mqttClient.init();
    }).catch((error) => {
      this.logger.error(error.stack || error);
      process.exit(1);
    });
  }

  /**
   * Produce a given message to a given topic.
   *
   * @param {string} topic
   * @param {Object} message
   *
   * @function sendMessage
   * @public
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

  /**
   * Health checking function to be passed to the ServiceStateManager.
   *
   * @param {Function} signalReady
   * @param {Function} signalNotReady
   *
   * @returns {Promise<void>}
   *
   * @function healthChecker
   * @public
   */
  healthChecker(signalReady, signalNotReady) {
    return new Promise((resolve) => {
      this.producer.getStatus()
        .then((status) => {
          if (status.connected) {
            signalReady();
          } else {
            signalNotReady();
          }
          return resolve();
        })
        .catch(() => {
          signalNotReady();
          return resolve();
        });
    });
  }

  /**
   * Shutdown handler to be passed to the ServiceStateManager.
   *
   * @returns {Promise<void>}
   *
   * @function shutdownHandler
   * @public
   */
  shutdownHandler() {
    this.logger.warn('Shutting down Kafka connection...');
    return this.producer.disconnect();
  }
}

module.exports = AgentMessenger;
