const { ConfigManager, Kafka: { Consumer }, Logger } = require('@dojot/microservice-sdk');

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
    if (!mqttClient) {
      throw new Error('no MQTT client was passed');
    }

    this.mqttClient = mqttClient;

    const config = ConfigManager.getConfig('K2V');
    this.consumeTopicSuffix = config.messenger['consume.topic.suffix'];

    this.consumer = new Consumer({
      ...config.sdk,
      'kafka.consumer': config.consumer,
      'kafka.topic': config.topic,
    });
    this.logger = new Logger('k2v:agent-messenger');

    this.wasInitialized = false;
  }

  /**
   * Initializes the agent messenger and registers callbacks for incoming messages.
   * @access public
   * @function init
   */
  init() {
    if (this.wasInitialized) {
      this.logger.debug('Kafka Consumer already online, skipping its initialization');
      return;
    }
    this.logger.info('Initializing Kafka Consumer...');
    this.consumer.init().then(() => {
      const topic = new RegExp(`^.+${this.consumeTopicSuffix.replace(/\./g, '\\.')}`);

      this.consumer.registerCallback(topic, (data) => {
        this.mqttClient.publishMessage(data);
      });

      this.wasInitialized = true;
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
