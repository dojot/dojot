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
   * Initialize the kafka and influxdb (including HeathChecker and Shutdown)
   */
  async init() {
    logger.info('init: Initializing the influxdb-storer ...');
    try {
      const influxIsReady = await this.influxDB.getInfluxStateInstance().isReady();
      if (!influxIsReady) {
        throw new Error('Influxdb is not ready');
      }

      this.influxDB.createHealthChecker();
      this.influxDB.registerShutdown();

      this.kafka.createHealthChecker();
      this.kafka.registerShutdown();

      // initializes kafka consumer
      await this.kafka.getKafkaConsumerInstance().init();

      // create the initial org, user and bucket
      await this.influxDB.getInfluxOrgInstance().initOnboarding();

      // associate by callback kafka consumers and influxdb actions
      this.initCallbacksTenantToCreateAndDelOrgs();
      this.initCallbacksDevicesCreateAndDelData();
    } catch (e) {
      logger.error('init:', e);
      throw e;
    }
    logger.info('init: ... service initialized.');
  }

  /**
   * Associates callbacks with kafka devices events to manipulation of influxdb data
   */
  initCallbacksDevicesCreateAndDelData() {
    // create callback to handle receive data
    const boundInfluxWriter = this.influxDB
      .getInfluxDataWriterInstance().write.bind(this.influxDB
        .getInfluxDataWriterInstance());

    const callbackWriteData = async (tenant, deviceid, timestamp, attrs) => {
      logger.debug('callbackWriteData: init');
      logger.debug(`callbackWriteData: tenant=${tenant}`);
      logger.debug(`callbackWriteData: deviceid=${deviceid}`);
      logger.debug(`callbackWriteData: timestamp=${timestamp}`);
      logger.debug(`callbackWriteData: attrs=${JSON.stringify(attrs)}`);
      try {
        await boundInfluxWriter(tenant, deviceid, attrs, timestamp);
      } catch (e) {
        // TODO: We need to think in a strategy when something like this happen.
        // One possibility would be to report some statistics like ratio of
        // dropped messages, other possibility would be to send the
        // unhandled messages or error notifications to a new topic of the Kafka.
        logger.error(`callbackWriteData: tenant=${tenant} deviceid=${deviceid} timestamp=${timestamp} attrs=${JSON.stringify(attrs)}`, e);
      }
    };

    // create callback to handle delete device
    const boundInfluxMeasurementDelete = this.influxDB
      .getInfluxMeasurementInstance()
      .deleteMeasurement.bind(this.influxMeasurement);
    const callbackDeleteMeasurement = async (tenant, deviceid) => {
      logger.debug('callbackDeleteMeasurement: init');
      logger.debug(`callbackDeleteMeasurement: tenant=${tenant}`);
      logger.debug(`callbackDeleteMeasurement: deviceid=${deviceid}`);
      try {
        await boundInfluxMeasurementDelete(tenant, deviceid);
      } catch (e) {
        // TODO: We need to think in a strategy when something like this happen.
        // One possibility would be to report some statistics like ratio of
        // dropped messages, other possibility would be to send the
        // unhandled messages or error notifications to a new topic of the Kafka.
        logger.error(`callbackDeleteMeasurement: tenant=${tenant} deviceid=${deviceid}`, e);
      }
    };

    this.kafka.getKafkaConsumerInstance().registerCallbacksForDeviceDataEvents(
      callbackWriteData,
    );

    this.kafka.getKafkaConsumerInstance().registerCallbacksForDeviceMgmtEvents(
      callbackWriteData,
      configDelete['device.data.enable'] ? callbackDeleteMeasurement : null,
    );
  }

  /**
   * Associates callbacks with kafka tenant events to manipulation of influxdb organizations
   */
  initCallbacksTenantToCreateAndDelOrgs() {
    // create callback to handle tenant create
    const boundCreateOrgWithBucket = this.influxDB
      .getInfluxOrgInstance().createOrgWithDefaultBucket.bind(
        this.influxDB.getInfluxOrgInstance(),
      );
    const callbackCreateTenant = async (tenant) => {
      logger.debug('callbackCreateTenant: init');
      logger.debug(`callbackCreateTenant: tenant=${tenant}`);
      try {
        await boundCreateOrgWithBucket(tenant);
      } catch (e) {
        // TODO: We need to think in a strategy when something like this happen.
        // One possibility would be to report some statistics like ratio of
        // dropped messages, other possibility would be to send the
        // unhandled messages or error notifications to a new topic of the Kafka.
        logger.error(`callbackCreateTenant: tenant=${tenant}`, e);
      }
    };

    // create callback to handle tenant delete
    const boundWriterCloseOrg = this.influxDB
      .getInfluxDataWriterInstance().closeOne.bind(this.influxDB.getInfluxDataWriterInstance());
    const boundDeleteOrgWithBucket = this.influxDB.getInfluxOrgInstance()
      .deleteOrg.bind(this.influxDB.getInfluxOrgInstance());
    const callbackDeleteTenant = async (tenant) => {
      logger.debug('callbackDeleteTenant: init');
      logger.debug(`callbackDeleteTenant: tenant=${tenant}`);
      try {
        await boundDeleteOrgWithBucket(tenant);
        await boundWriterCloseOrg(tenant);
      } catch (e) {
        // TODO: We need to think in a strategy when something like this happen.
        // One possibility would be to report some statistics like ratio of
        // dropped messages, other possibility would be to send the
        // unhandled messages or error notifications to a new topic of the Kafka.
        logger.error(`callbackDeleteTenant: tenant=${tenant}`, e);
      }
    };

    this.kafka.getKafkaConsumerInstance().registerCallbackForTenantEvents(
      callbackCreateTenant,
      configDelete['tenant.data.enable'] ? callbackDeleteTenant : null,
    );
  }
}

module.exports = App;
