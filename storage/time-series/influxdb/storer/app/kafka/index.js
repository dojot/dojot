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
class DojotKafka {
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

    this.callbacksConsumerDevice = {
      dataDevice: null,
      deleteDevice: null,
    };

    this.callbacksConsumerTenant = {
      createTenant: null,
      deleteTenant: null,
    };
  }

  /**
   * Initialize kafka
   */
  async init() {
    await this.kafkaConsumer.init();
  }

  /**
   * Set callbacks to handle device events
   *
   * @param {async function(string, string, number|string, object)} writeDeviceData
   *                              Receive tenant, deviceid, date-time (unix timestamp ms
   *                              or RFC3339,
   *                              attrs (key:value)
   * @param {async function(string, string)} deleteDevice=null Receive tenant,
   *                                         deviceid (default = null)
   */
  setCallbacksConsumerDevice(dataDevice, deleteDevice = null) {
    this.callbacksConsumerDevice.dataDevice = dataDevice;
    this.callbacksConsumerDevice.deleteDevice = deleteDevice;
  }

  /**
   *Set callbacks to handle tenant events
   *
   * @param {async function(string)} createTenant Receive the tenant name created
   * @param {async function(string)} deleteTenant=null Receive the tenant name deleted
   */
  setCallbacksConsumerTenant(createTenant, deleteTenant = null) {
    this.callbacksConsumerTenant.createTenant = createTenant;
    this.callbacksConsumerTenant.deleteTenant = deleteTenant;
  }

  /**
   *  Returns a KafkaConsume instance
   * @returns {KafkaConsume}
   */
  getKafkaConsumerInstance() {
    return this.kafkaConsumer;
  }

  /**
  * Register consumer callbacks that were set in  setCallbacksConsumerDevice
  * and setCallbacksConsumerTenant
  */
  registerCallbacksConsumer() {
    this.kafkaConsumer
      .registerCallbacksForDeviceDataEvents(this.callbacksConsumerDevice.dataDevice);

    this.kafkaConsumer.registerCallbacksForDeviceMgmtEvents(this.callbacksConsumerDevice.dataDevice,
      this.callbacksConsumerDevice.deleteDevice);
    this.kafkaConsumer.registerCallbackForTenantEvents(this.callbacksConsumerTenant.createTenant,
      this.callbacksConsumerTenant.deleteTenant);
  }

  /**
  * Unregister consumer callbacks that were set in  setCallbacksConsumerDevice
  * and setCallbacksConsumerTenant
  */
  unregisterCallbacksConsumer() {
    this.kafkaConsumer.unregisterCallbacks();
  }

  /**
  * Creates a 'healthCheck'
  */
  createHealthChecker() {
    const kafkaHealthChecker = async (signalReady, signalNotReady) => {
      const isConnected = await this.kafkaConsumer.isConnected();
      if (isConnected) {
        logger.debug('createHealthChecker: Kafka is healthy');
        signalReady();
      } else {
        logger.warn('createHealthChecker: Kafka is not healthy');
        signalNotReady();
      }
    };
    this.serviceState.addHealthChecker(
      'kafka', kafkaHealthChecker, configKafka['heathcheck.ms'],
    );
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

module.exports = DojotKafka;
