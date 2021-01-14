const { Logger } = require('@dojot/microservice-sdk');

const { Kafka: { Consumer } } = require('@dojot/microservice-sdk');
const { kafka: kafkaConfig } = require('../Config');

const logger = new Logger('kafka-ws:kafka-consumer');

/**
 * Consume kafka messages from kafka topics
 */
class KafkaConsumer {
  /**
  * Instance KafkaConsumer
  */
  constructor() {
    logger.debug('constructor: Instance KafkaConsumer');
    this.consumer = new Consumer(kafkaConfig);
    // only one callback by topic
    this.registeredCallbacks = new Map();
  }

  /**
  * Inicialize Kafka consumer
  */
  async init() {
    try {
      logger.info('init: Kafka starting...');
      await this.consumer.init();
      logger.info('init: ...Kafka started ');
    } catch (error) {
      logger.error(`init: Error starting kafka ${error.stack}`);
      throw error;
    }
  }

  /**
   * Register Callback for a topic
   *
   * @param {string} kafkaTopic
   * @param {function} callback
  */
  registerCallback(kafkaTopic, callback) {
    if (!this.registeredCallbacks.has(kafkaTopic)) {
      logger.debug(`registerCallback: Register Callback for topic ${kafkaTopic}`);
      const idCallback = this.consumer.registerCallback(kafkaTopic, callback);
      this.registeredCallbacks.set(kafkaTopic, idCallback);
    } else {
      throw new Error(`registerCallback: Callback for topic ${kafkaTopic} already exist`);
    }

    logger.debug(`registerCallback: All Registered Callbacks ${[...this.registeredCallbacks]}`);
  }

  /**
   * Unregister Callback for a topic
   * @param {string} kafkaTopic
   */
  unregisterCallback(kafkaTopic) {
    if (this.registeredCallbacks.has(kafkaTopic)) {
      logger.debug(`unregisterCallbacks: Unregister Callback for topic ${kafkaTopic}`);
      const idCallback = this.registeredCallbacks.get(kafkaTopic);
      this.consumer.unregisterCallback(idCallback);
      this.registeredCallbacks.delete(kafkaTopic);
    } else {
      throw new Error(`unregisterCallbacks: Doesn't exist Callback to unregister for topic ${kafkaTopic}`);
    }

    logger.debug(`unregisterCallbacks: All Registered Callbacks ${[...this.registeredCallbacks]}`);
  }

  /**
  * Unregister All Callbacks
  */
  unregisterAllCallbacks() {
    logger.debug('unregisterCallbacks: Unregister All Callbacks');

    this.registeredCallbacks.forEach((idCallback) => {
      this.consumer.unregisterCallback(idCallback);
    });

    this.registeredCallbacks.clear();
  }
}

module.exports = KafkaConsumer;
