const {
  ConfigManager: { getConfig },
  Logger, Kafka: { Consumer },
} = require('@dojot/microservice-sdk');
const StateManager = require('../StateManager');

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
      ...this.config.kafka,
      'kafka.consumer': { ...this.config.consumer },
      'kafka.topic': { ...this.config.topic },
    };

    this.consumer = new Consumer(this.consumerConfig);
    this.registeredCallbacks = new Map();

    // healthchecker and shutdown
    const healthCheckerInterval = this.config.healthcheck['kafka.interval.ms'];
    StateManager.addHealthChecker('kafka', this.checkHealth.bind(this), healthCheckerInterval);
    StateManager.registerShutdownHandler(this.shutdownProcess.bind(this));
  }

  /**
   * Initialize the kafka consumer
   */
  async init() {
    logger.info('init: kafka starting...');
    return this.consumer.init();
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
