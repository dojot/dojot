const { Kafka: { Consumer }, Logger } = require('@dojot/microservice-sdk');

const config = require('./config');
const utils = require('./utils');

/**
 * An agent to consume messages from pre-defined topics in Apache Kafka,
 * format them and publish the formatted messages to pre-defined topics in VerneMQ
 * @class
 */
class AgentMessenger {
  /**
   * Creates an Agent Messenger
   * @access public
   * @constructor
   *
   * @param {Object} mqttClient - a mqtt client to connect to VerneMQ broker.
   */
  constructor(mqttClient) {
    this.mqttClient = mqttClient;
    this.consumer = new Consumer({ ...config.sdk, kafka: config.kafka });
    this.logger = new Logger('AgentMessenger');
  }

  /**
   * Initializes the agent messenger and registers callbacks for incoming messages.
   * @access public
   * @function init
   */
  init() {
    this.logger.info('Initializing Kafka Consumer...');
    this.consumer.init().then(() => {
      const topic = new RegExp(`^.+${config.messenger['consume.topic.suffix'].replace(/\./g, '\\.')}`);

      this.consumer.registerCallback(topic, (data) => {
        this.mqttClient.publishMessage(data);
      });

      this.logger.info('... Kafka Consumer was initialized');
    }).catch((error) => {
      this.logger.error('Error while initializing the Kafka Consumer');
      if (error) {
        this.logger.error(error.stack || error);
      }
      this.logger.error('Bailing out!');
      utils.killApplication();
    });
  }
}

module.exports = AgentMessenger;
