const {
  ServiceStateManager,
  ConfigManager: { getConfig, transformObjectKeys },
  Logger,
} = require('@dojot/microservice-sdk');

const camelCase = require('lodash.camelcase');
const InfluxDB = require('./influx');
const Kafka = require('./kafka');

const {
  delete: configDelete, lightship: configLightship,
} = getConfig('STORER');


const serviceState = new ServiceStateManager({
  lightship: transformObjectKeys(configLightship, camelCase),
});
serviceState.registerService('influxdb');
serviceState.registerService('kafka');

const logger = new Logger('influxdb-storer:App');

/**
* Wrapper to initialize the service
*/
class App {
  /**
    * Constructor App
    * that instantiate all kafka and influxdb classes
    */
  constructor() {
    logger.debug('constructor: instantiate app...');
    try {
      this.influxDB = new InfluxDB(serviceState);
      this.kafka = new Kafka(serviceState);
    } catch (e) {
      logger.error('constructor:', e);
      throw e;
    }
  }

  /**
   * Initializes the kafka and influxdb (including HeathChecker and Shutdown)
   */
  async init() {
    logger.info('init: Initializing the influxdb-storer ...');
    try {
      const influxIsReady = await this.influxDB.getInfluxStateInstance().isReady();
      if (!influxIsReady) {
        throw new Error('InfluxDB is not ready');
      }

      const boundKafkaRegisterCallbacksConsumer = this.kafka
        .registerCallbacksConsumer.bind(this.kafka);

      const boundKafkaUnregisterCallbacksConsumer = this.kafka
        .unregisterCallbacksConsumer.bind(this.kafka);


      // create health check to know if it is possible to connect with kafka
      this.kafka.createHealthChecker();
      // defines shutdown behavior for kafka
      // All registered shutdown handlers are
      // executed in the order they have been registered.
      this.kafka.registerShutdown();

      // when the influxdb service is unhealthy the kafka consumer
      // callbacks will be unregistered and when the service becomes healthy
      // they will be registered again.
      this.influxDB.createHealthChecker(boundKafkaRegisterCallbacksConsumer,
        boundKafkaUnregisterCallbacksConsumer);
      // defines shutdown behavior for influx
      // All registered shutdown handlers are
      // executed in the order they have been registered.
      this.influxDB.registerShutdown();

      // initializes kafka
      await this.kafka.init();

      // associate by callback kafka consumers and influxdb actions
      this.setCallbacksKafkaConsumerTenant();
      this.setCallbacksKafkaConsumerDevice();

      // register callbacks that were set in kafka
      this.kafka.registerCallbacksConsumer();
    } catch (e) {
      logger.error('init:', e);
      throw e;
    }
    logger.info('init: ... service initialized.');
  }

  /**
   * Associates callbacks with kafka devices events to manipulation of influxdb data
   */
  setCallbacksKafkaConsumerDevice() {
    // create callback to handle receive data
    const callbackWriteData = async (
      tenant, deviceid, timestamp, attrs,
    ) => {
      logger.debug('callbackWriteData: init');
      logger.debug(`callbackWriteData: tenant=${tenant}`);
      logger.debug(`callbackWriteData: deviceid=${deviceid}`);
      logger.debug(`callbackWriteData: timestamp=${timestamp}`);
      logger.debug(`callbackWriteData: attrs=${JSON.stringify(attrs)}`);
      try {
        await this.influxDB
          .getInfluxDataWriterInstance().write(
            tenant, deviceid, attrs, timestamp,
          );
      } catch (e) {
        // TODO: We need to think in a strategy when something like this happen.
        // One possibility would be to report some statistics like ratio of
        // dropped messages, other possibility would be to send the
        // unhandled messages or error notifications to a new topic of the Kafka.
        logger.error(`callbackWriteData: tenant=${tenant} deviceid=${deviceid} timestamp=${timestamp} attrs=${JSON.stringify(attrs)}`, e);
      }
    };

    // create callback to handle delete device
    const callbackDeleteMeasurement = async (tenant, deviceid) => {
      logger.debug('callbackDeleteMeasurement: init');
      logger.debug(`callbackDeleteMeasurement: tenant=${tenant}`);
      logger.debug(`callbackDeleteMeasurement: deviceid=${deviceid}`);
      try {
        await this.influxDB
          .getInfluxMeasurementInstance()
          .deleteMeasurement(tenant, deviceid);
      } catch (e) {
        // TODO: We need to think in a strategy when something like this happen.
        // One possibility would be to report some statistics like ratio of
        // dropped messages, other possibility would be to send the
        // unhandled messages or error notifications to a new topic of the Kafka.
        logger.error(`callbackDeleteMeasurement: tenant=${tenant} deviceid=${deviceid}`, e);
      }
    };

    this.kafka.setCallbacksConsumerDevice(callbackWriteData,
      configDelete['device.data.enable'] ? callbackDeleteMeasurement : null);
  }

  /**
   * Associates callbacks with kafka tenant events to manipulation of influxdb organizations
   */
  setCallbacksKafkaConsumerTenant() {
    // create callback to handle tenant create
    const callbackCreateTenant = async (tenant) => {
      logger.debug('callbackCreateTenant: init');
      logger.debug(`callbackCreateTenant: tenant=${tenant}`);
      try {
        await this.influxDB
          .getInfluxOrgInstance().createOrgWithDefaultBucket(tenant);
      } catch (e) {
        // TODO: We need to think in a strategy when something like this happen.
        // One possibility would be to report some statistics like ratio of
        // dropped messages, other possibility would be to send the
        // unhandled messages or error notifications to a new topic of the Kafka.
        logger.error(`callbackCreateTenant: tenant=${tenant}`, e);
      }
    };

    // create callback to handle tenant delete
    const callbackDeleteTenant = async (tenant) => {
      logger.debug('callbackDeleteTenant: init');
      logger.debug(`callbackDeleteTenant: tenant=${tenant}`);
      try {
        await this.influxDB.getInfluxOrgInstance()
          .deleteOrg(tenant);
        await this.influxDB
          .getInfluxDataWriterInstance().closeOne(tenant);
      } catch (e) {
        // TODO: We need to think in a strategy when something like this happen.
        // One possibility would be to report some statistics like ratio of
        // dropped messages, other possibility would be to send the
        // unhandled messages or error notifications to a new topic of the Kafka.
        logger.error(`callbackDeleteTenant: tenant=${tenant}`, e);
      }
    };

    this.kafka.setCallbacksConsumerTenant(callbackCreateTenant,
      configDelete['tenant.data.enable'] ? callbackDeleteTenant : null);
  }
}

module.exports = App;
