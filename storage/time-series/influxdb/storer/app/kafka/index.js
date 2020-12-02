const {
  ConfigManager: { getConfig },
  Logger,
} = require('@dojot/microservice-sdk');
const KafkaConsumer = require('./DojotConsumer');

const { kafka: configKafka } = getConfig('STORER');
const logger = new Logger('influxdb-storer:Influx');

/**
 * Wrapper for Kafka
 */
class Kafka {
  /**
   * @constructor
   *
 * @param {an instance of @dojot/microservice-sdk.ServiceStateManager
 *          with register service 'kafka'} serviceState
 *          Manages the services' states, providing health check and shutdown utilities.
   */
  constructor(serviceState) {
    this.kafkaConsumer = new KafkaConsumer();
    this.serviceState = serviceState;
  }


  /**
   *  Returns a KafkaConsume instance
   * @returns {KafkaConsume}
   */
  getKafkaConsumerInstance() {
    return this.kafkaConsumer;
  }

  /**
 * Creates a 'healthCheck'
 */
  createHealthChecker() {
    const kafkaHealthChecker = async (signalReady, signalNotReady) => {
      const isConnected = await this.kafkaConsumer.isConnected();
      if (isConnected) {
        logger.debug('kafkaHealthChecker: Server is healthy');
        signalReady();
      } else {
        logger.warn('kafkaHealthChecker: Server is not healthy');
        signalNotReady();
      }
    };
    this.serviceState.addHealthChecker('kafka', kafkaHealthChecker, configKafka['heathcheck.ms']);
  }


  /**
   *  Registers a shutdown
   */
  async registerShutdown() {
    this.serviceState.registerShutdownHandler(async () => {
      logger.debug('ShutdownHandler: Trying Unregister Callback for kafka consumer...');
      this.kafkaConsumer.unregisterCallbacks();
      logger.warn('ShutdownHandler: Unregistered callback for kafka consumer.');
      logger.debug('ShutdownHandler: Trying finish kafka consumer...');
      await this.kafkaConsumer.finish();
      logger.warn('ShutdownHandler: Finished kafka consumer.');
    });
  }
}

module.exports = Kafka;
