const { Logger } = require('@dojot/microservice-sdk');

const { ConfigManager, Kafka: { Consumer } } = require('@dojot/microservice-sdk');
const StateManager = require('../StateManager');

const logger = new Logger('kafka-ws:kafka-consumer');

const KAFKA_WS_CONFIG_LABEL = 'KAFKA_WS';

/**
 * Consume kafka messages from kafka topics
 */
class KafkaConsumer {
  /**
  * Instance KafkaConsumer
  */
  constructor() {
    this.config = ConfigManager.getConfig(KAFKA_WS_CONFIG_LABEL);

    const consumerConfig = {
      'kafka.consumer': { ...this.config.consumer },
      'kafka.topic': { ...this.config.topic },
    };
    logger.debug('constructor: Instance KafkaConsumer');
    this.consumer = new Consumer(consumerConfig);
    // only one callback by topic
    this.registeredCallbacks = new Map();

    // healthChecker
    this.healthCheckerBind = this.healthChecker.bind(this);
    StateManager.registerService('kafka');
    StateManager.addHealthChecker('kafka', this.healthCheckerBind, 5000);
  }

  /**
  * Initialize Kafka consumer
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

  healthChecker(signalReady, signalNotReady) {
    this.consumer.getStatus().then((data) => {
      if (data.connected) {
        signalReady();
      } else {
        signalNotReady();
      }
    }).catch((err) => {
      signalNotReady();
      logger.warn(`Error ${err}`);
    });
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
