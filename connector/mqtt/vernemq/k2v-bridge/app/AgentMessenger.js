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
   *
   * @param {Object} serviceStateManager
   *
   * @constructor
   */
  constructor(serviceStateManager) {
    if (!serviceStateManager) {
      throw new Error('no ServiceStateManager instance was passed');
    }

    this.config = ConfigManager.getConfig('K2V');
    this.consumerTopicSuffix = this.config.messenger['consumer.topic.suffix'];

    this.logger = new Logger('k2v:agent-messenger');

    this.stateService = 'kafka';
    this.serviceStateManager = serviceStateManager;

    this.consumer = undefined;
    this.wasInitialized = false;
  }

  /**
   * Initializes the agent messenger and registers callbacks for incoming messages.
   *
   * @param {Object} mqttClient
   *
   * @return {Promise<any>}
   *
   * @function init
   * @public
   */
  async init(mqttClient) {
    if (this.wasInitialized) {
      this.logger.debug('Kafka Consumer already online, skipping its initialization');
      return;
    }
    this.logger.info('Initializing Kafka Consumer...');

    try {
      this.consumer = new Consumer({
        ...this.config.sdk,
        'kafka.consumer': this.config.consumer,
        'kafka.topic': this.config.topic,
      });
      await this.consumer.init();
      const topic = new RegExp(`^.+${this.consumerTopicSuffix.replace(/\./g, '\\.')}`);

      this.consumer.registerCallback(topic, mqttClient.publishMessage.bind(mqttClient));

      this.serviceStateManager.signalReady(this.stateService);
      this.wasInitialized = true;
      this.logger.info('... Kafka Consumer was initialized');
    } catch (error) {
      this.serviceStateManager.signalNotReady(this.stateService);
      this.logger.error('Error while initializing the Kafka Consumer');
      if (error) {
        this.logger.error(error.stack || error);
      }
      this.logger.error('Bailing out!');
      utils.killApplication();
    }
  }

  /**
   * Stops Kafka message consumption.
   *
   * @returns {Promise<any>}
   */
  async finish() {
    try {
      this.wasInitialized = false;
      await this.consumer.finish();
      this.consumer = undefined;
    } catch (error) {
      this.logger.debug('Error while finishing Kafka connection, going on like nothing happened');
    }
    this.serviceStateManager.signalNotReady(this.stateService);
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
  async healthChecker(signalReady, signalNotReady) {
    if (this.consumer) {
      try {
        const status = await this.consumer.getStatus();
        if (status.connected) {
          signalReady();
        } else {
          signalNotReady();
        }
      } catch (error) {
        signalNotReady();
      }
    } else {
      signalNotReady();
    }
  }

  /**
   * Shutdown handler to be passed to the ServiceStateManager.
   *
   * @returns {Promise<void>}
   *
   * @function shutdownHandler
   * @public
   */
  async shutdownHandler() {
    this.logger.warn('Shutting down Kafka connection...');
    await this.finish();
  }
}

module.exports = AgentMessenger;
