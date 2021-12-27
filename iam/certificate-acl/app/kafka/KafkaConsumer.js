const {
  ConfigManager: { getConfig },
  Logger,
  Kafka: { Consumer },
} = require('@dojot/microservice-sdk');

const logger = new Logger('certificate-acl:kafka-consumer');

const CERTIFICATE_ACL_CONFIG_LABEL = 'CERTIFICATE_ACL';

/**
 * Consumes messages from predefined topics
 */
class kafkaConsumer {
  /**
   * Creates a high-level Kafka Consumer wrapper
   *
   * @param {*} serviceStateManager instance of a ServiceStateManager
   * @returns KafkaConsumer instance
   */
  constructor(serviceStateManager) {
    const config = getConfig(CERTIFICATE_ACL_CONFIG_LABEL);
    this.consumer = new Consumer({
      ...config.kafka,
      'kafka.consumer': { ...config.consumer },
      'kafka.topic': { ...config.topic },
    });

    // true whether consumption is suspended; otherwise, false
    this.suspended = false;

    // <topic> -> {id: <callbackId>, cb: <callback> }
    this.registeredCallbacks = new Map();

    // health-check and shutdown
    this.healthy = false;
    this.serviceStateManager = serviceStateManager;
    this.serviceStateManager.registerService('kafka');
    this.serviceStateManager.addHealthChecker(
      'kafka',
      this.checkHealth.bind(this),
      config.healthcheck['kafka.interval.ms'],
    );
    this.serviceStateManager.registerShutdownHandler(this.shutdown.bind(this));
    Object.seal(this);

    logger.info('Kafka Consumer created!');
    logger.info(`Kafka Consumer configuration: ${JSON.stringify(config.kafka)}`);
  }

  /**
   * Initializes the kafka consumer
   */
  async init() {
    return this.consumer.init();
  }

  /**
   * Registers a single processing callback for a given topic
   *
   * @param {string} kafkaTopic an explicit topic or a regex for a topic
   * @param {function} callback function to process topic's messages
   */
  registerCallback(kafkaTopic, callback) {
    if (!this.registeredCallbacks.has(kafkaTopic)) {
      logger.debug(`Registering processing callback for topic ${kafkaTopic}`);
      const cbId = this.suspended ? null
        : this.consumer.registerCallback(kafkaTopic, callback);
      this.registeredCallbacks.set(kafkaTopic, { id: cbId, cb: callback });
    } else {
      throw new Error(`Failed: Callback for topic ${kafkaTopic} already exist`);
    }
  }

  /**
   * It suspends all registered processing callbacks
   *
   * The processing of the messages will be interrupted until
   * resumeCallbacks() is called.
   *
   * Once the service keeps connected with Kafka, the messages won't
   * be consumed by another instance in the same consumer group. So,
   * if the problem is not ephemeral, it is better to disconnect from
   * the Kafka to allow other instances of the same consumer group to
   * consume the messages.
   *
   * TODO: move to sdk
   */
  suspend() {
    // Callbacks have already been suspended.
    // Nothing to be done
    if (this.suspended) return;

    logger.info('Suspending all processing callbacks.');
    this.suspended = true;
    this.registeredCallbacks.forEach((value, key) => {
      if (value.id) {
        this.consumer.unregisterCallback(value.id);
        logger.info(`Suspended processing callback for topic: ${key}.`);
        // eslint-disable-next-line no-param-reassign
        value.id = null;
      }
    });
  }

  /**
   * It resumes all registered processing callbacks
   *
   * The processing of the messages will be resumed.
   *
   * TODO: move to sdk
   */
  resume() {
    // Callbacks haven't been suspended
    // Nothing to be done
    if (!this.suspended) return;

    logger.info('Resuming all processing callbacks ...');
    this.registeredCallbacks.forEach((value, key) => {
      if (value.id === null) {
        // eslint-disable-next-line no-param-reassign
        value.id = this.consumer.registerCallback(key, value.cb);
        logger.info(`Resumed processing callback for topic: ${key}.`);
      }
    });
    this.suspended = false;
  }

  /**
   * Health-check function to be passed to the ServiceStateManager
   *
   * It monitors the connection with the Kafka Server.
   *
   * Note: This strategy is very naive and can produce false-positive
   * responses. A better approach would be checking if messages are being
   * consumed and committed successfully and only use this one when messages
   * are not being received.
   *
   * TODO: Create a robust health check strategy for Kafka clients.
   * @param {function} signalReady
   * @param {function} signalNotReady
   *
   */
  checkHealth(signalReady, signalNotReady) {
    return this.consumer.getStatus().then((data) => {
      if (data.connected && !this.healthy) {
        this.healthy = true;
        signalReady();
        logger.info('Kafka Consumer is healthy (Connected).');
      } else if (!data.connected) {
        if (this.healthy) {
          this.healthy = false;
          signalNotReady();
        }
        logger.warn('Kafka Consumer is unhealthy (Reconnecting?).');
      }
    }).catch((err) => {
      if (this.healthy) {
        this.healthy = false;
        signalNotReady();
        logger.warn('Kafka Consumer is unhealthy (Unknown).');
      }
      logger.warn(`Failed health check. Error:${err}.`);
    });
  }

  /**
   * Shutdown function to be passed to the ServiceStateManager
   *
   * It tries to terminate the kafka consumer gracefully.
   *
   */
  shutdown() {
    return this.consumer.finish().then(() => {
      logger.warn('Kafka consumer finished!');
    });
  }
}

module.exports = kafkaConsumer;
