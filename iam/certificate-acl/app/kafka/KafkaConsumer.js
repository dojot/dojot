const {
  ConfigManager: { getConfig },
  Logger, Kafka: { Consumer },
} = require('@dojot/microservice-sdk');

const logger = new Logger('certificate-acl:kafka-consumer');

const CERTIFICATE_ACL_CONFIG_LABEL = 'CERTIFICATE_ACL';

/**
 * Consumes messages from predefined topics
 */
class kafkaConsumer {
  /**
   * Create an Kafka Consumer
   */
  constructor() {
    this.config = getConfig(CERTIFICATE_ACL_CONFIG_LABEL);

    this.consumerConfig = {
      'kafka.consumer': { ...this.config.consumer },
      'kafka.topic': { ...this.config.topic },
    };

    this.consumer = new Consumer(this.consumerConfig);
    this.registeredCallbacks = new Map();
  }

  /**
   * Initialize the kafka consumer
   */
  async init() {
    try {
      logger.info('init: kafka starting...');
      await this.consumer.init();
      logger.info('init: kafka started...');
    } catch (error) {
      logger.error(`init: Error starting kafka ${error.stack}`);
      throw error;
    }
  }

  /**
   * Register callback for a topic
   *
   * @param {string} kafkaTopic
   * @param {function} callback
   */
  registerCallback(kafkaTopic, callback) {
    if (!this.registeredCallbacks.has(kafkaTopic)) {
      logger.debug(`registerCallback: Register Callback for topic ${kafkaTopic}`);
      const callbackId = this.consumer.registerCallback(kafkaTopic, callback);
      this.registeredCallbacks.set(kafkaTopic, callbackId);
    } else {
      throw new Error(`registerCallback: Callback for topic ${kafkaTopic} already exist`);
    }

    logger.debug(`registerCallback: All Registered Callbacks ${[...this.registeredCallbacks]}`);
  }

  /**
   * @function unregisterAllCallback
   *
   * Unregister Callback for a given topic
   *
   * @param {string} kafkaTopic
   */
  unregisterCallback(kafkaTopic) {
    if (this.registeredCallbacks.has(kafkaTopic)) {
      logger.debug(`unregisterCallbacks: Unregister Callback for topic ${kafkaTopic}`);
      const callbackId = this.registeredCallbacks.get(kafkaTopic);
      this.consumer.unregisterCallback(callbackId);
      this.registeredCallbacks.delete(kafkaTopic);
    } else {
      throw new Error(`unregisterCallbacks: Doesn't exist Callback to unregister for topic ${kafkaTopic}`);
    }
  }

  /**
   * @function unregisterAllCallbacks
   *
   * Unregister all callbacks
   */
  unregisterAllCallbacks() {
    logger.debug('unregisterCallbacks: Unregister All Callbacks');

    this.registeredCallbacks.forEach((callbackId) => this.consumer.unregisterCallback(callbackId));
    this.registeredCallbacks.clear();
  }

  /**
   * HealthChecker to be passed to the ServiceStateManager
   *
   * @param {function} signalReady
   * @param {function} signalNotReady
   */
  checkHealth(signalReady, signalNotReady) {
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
   * @function shutdownProcess
   *
   * Shutdown handler to be passed to the ServiceStateManager
   */
  shutdownProcess() {
    return this.consumer.finish().then(() => {
      logger.warn('Kafka Consumer finished!');
    });
  }
}

module.exports = kafkaConsumer;
