const { Logger, ConfigManager, Kafka: { Consumer } } = require('@dojot/microservice-sdk');

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

    this.isKafkaAvailable = false;
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

  /**
   * Get kafka Status for internal uses
   */
  getKafkaStatus() {
    return this.isKafkaAvailable;
  }

  /**
   * HealthChecker to be passed to the ServiceStateManager
   *
   * @param {*} signalReady
   * @param {*} signalNotReady
   */
  checkHealth(signalReady, signalNotReady) {
    this.consumer.getStatus().then((data) => {
      if (data.connected) {
        signalReady();
        this.isKafkaAvailable = true;
        logger.debug('Kafka is health');
      } else {
        signalNotReady();
        this.isKafkaAvailable = false;
        logger.warn('Kafka is not health');
      }
    }).catch((err) => {
      this.isKafkaAvailable = false;
      signalNotReady();
      logger.warn(`Error ${err}`);
    });
  }

  /**
   *  Shutdown handler to be passed to the ServiceStateManager.
   */
  async shutdownProcess() {
    logger.info('Finishing kafka consumer ....');
    await new Promise((resolve) => {
      this.consumer.finish().then(() => {
        resolve('Kafka Consumer finished!');
      });
    });

    await new Promise((resolve) => {
      setImmediate(resolve);
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
