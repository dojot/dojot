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
   * @param {@dojot/microservice-sdk
   *                  .ServiceStateManager} serviceState Manages the services' states,
   *                                    providing health check and shutdown utilities.
   */
  constructor(serviceState) {
    this.kafkaConsumer = new KafkaConsumer();
    this.serviceState = serviceState;
    this.serviceState.registerService('kafka');
  }

  /**
   *  Returns a KafkaConsume instance
   * @returns {KafkaConsume}
   */
  getKafkaConsumerInstance() {
    return this.kafkaConsumer;
  }

  /**
 * Create a 'healthCheck'
 */
  createHealthChecker() {
    const boundIsConnectedKafka = this.kafkaConsumer
      .isConnected.bind(this.kafkaConsumer);
    const kafkaHealthChecker = async (signalReady, signalNotReady) => {
      const isConnected = await boundIsConnectedKafka();
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
   *  Register a shutdown
   */
  async registerShutdown() {
    const boundKafkaUnregisterCallbacks = this.kafkaConsumer
      .unregisterCallbacks.bind(this.kafkaConsumer);

    const boundKafkaFinish = this.kafkaConsumer
      .finish.bind(this.kafkaConsumer);

    this.serviceState.registerShutdownHandler(async () => {
      logger.debug('ShutdownHandler: Trying Unregister Callback for kafka consumer...');
      await boundKafkaUnregisterCallbacks();
      logger.warn('ShutdownHandler: Unregistered callback for kafka consumer.');
    });

    // TODO: make sure it 'finish' really works because it doesn't seem to work ...
    this.serviceState.registerShutdownHandler(async () => {
      logger.debug('ShutdownHandler: Trying finish kafka consumer...');
      await boundKafkaFinish();
      logger.warn('ShutdownHandler: Finished kafka consumer.');
    });
  }
}

module.exports = Kafka;
