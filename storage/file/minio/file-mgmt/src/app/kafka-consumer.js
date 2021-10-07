const {
  Kafka: { Consumer },
} = require('@dojot/microservice-sdk');

/**
 * This class handles messages from dojot topics on kafka
 * @class
 */
class KafkaConsumer extends Consumer {
  /**
   * Create an instance
   */
  constructor(config, logger) {
    logger.debug('constructor: Instantiating a Kafka Consumer');

    super({
      ...config.sdk,
      'enable.async.commit': true,
      'kafka.consumer': config.consumer,
      'kafka.topic': config.topic,
    });

    this.idCallbackTenant = null;
    this.logger = logger;
  }

  init(consumers) {
    super.init();
    this.registerTopics(consumers);
  }

  registerTopics(consumers) {
    consumers.forEach((consumer) => {
      this.registerTopic(consumer.topicSuffix, consumer.handler);
    });
  }

  registerTopic(topicSuffix, handler) {
    const topic = new RegExp(`^.+${topicSuffix.replace(/\./g, '\\.')}`);

    this.idCallbackTenant = this.registerCallback(
      topic, handler,
    );
    this.logger.debug('registerCallback: Registered Callback');
  }

  /**
     * A function to get if kafka is connected
     *
     * @returns {Promise<boolean>} if kafka is connect
     */
  async isConnected() {
    try {
      const { connected } = await this.getStatus();
      if (connected) {
        return true;
      }
      return false;
    } catch (e) {
      this.logger.error('isConnected:', e);
      return false;
    }
  }

  /**
       * Unregister all callbacks
       *
       * @throws If Cannot unregister callback
       */
  unregisterCallbacks() {
    if (this.idCallbackTenant) {
      this.unregisterCallback(this.idCallbackTenant);
      this.idCallbackTenant = null;
      this.logger.debug('unregisterCallbacks: Unregistered callback for tenant');
    } else {
      this.logger.warn('unregisterCallbacks: Doesn\'t exist Callback to unregister for tenant');
    }
  }
}

module.exports = KafkaConsumer;
