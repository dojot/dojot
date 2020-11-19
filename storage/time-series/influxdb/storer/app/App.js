const flatten = require('flat');
const {
  ServiceStateManager,
  ConfigManager: { getConfig, transformObjectKeys },
  Logger,
} = require('@dojot/microservice-sdk');

const camelCase = require('lodash.camelcase');
const InfluxState = require('./influx/State');
const KafkaConsumer = require('./kafka/DojotConsumer');
const InfluxWriter = require('./influx/WriterData');
const InfluxOrgs = require('./influx/Organizations');
const InfluxMeasurement = require('./influx/Measurements');

const { influx: configInflux, delete: configDelete, lightship: configLightship } = getConfig('STORER');

const { write: { options: influxWriteOptions } } = flatten.unflatten(configInflux);
const configInfluxWriteOptions = influxWriteOptions ? flatten(influxWriteOptions) : {};
const configInfluxWriteOptionsCamelCase = transformObjectKeys(configInfluxWriteOptions, camelCase);

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
      // TODO create a influx class to wrapper influx classes
      this.kafkaConsumer = new KafkaConsumer();
      this.influxOrgs = new InfluxOrgs(
        configInflux.url,
        configInflux['default.token'],
        configInflux['default.user'],
        configInflux['default.password'],
        configInflux['default.organization'],
        configInflux['default.bucket'],
        configInflux['retention.hrs'],
      );
      this.influxWriter = new InfluxWriter(
        configInflux.url,
        configInflux['default.token'],
        configInflux['default.bucket'],
        configInfluxWriteOptionsCamelCase,
      );
      this.influxMeasurement = new InfluxMeasurement(
        configInflux.url,
        configInflux['default.token'],
        configInflux['default.bucket'],
      );
      this.influxState = new InfluxState(
        configInflux.url,
      );
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
      serviceState.registerService('kafka');
      serviceState.registerService('influxdb');
      this.createHealthCheckers();
      this.defineShutdown();

      const influxIsReady = await this.influxState.isReady();
      if (!influxIsReady) {
        throw new Error('Influxdb is not ready');
      }

      serviceState.signalReady('influxdb');

      // initializes kafka consumer
      await this.kafkaConsumer.init();

      // const kafkaIsConnected = await this.kafkaConsumer.isConnected();
      // if (!kafkaIsConnected) {
      //   throw new Error('Kafka is not ready');
      // }
      // serviceState.signalReady('kafka');

      // create the initial org, user and bucket
      await this.influxOrgs.initOnboarding();

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
    const boundInfluxWriter = this.influxWriter.writer.bind(this.influxWriter);
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
    const boundInfluxMeasurementDelete = this.influxMeasurement
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

    this.kafkaConsumer.registerCallbacksForDeviceDataEvents(
      callbackWriteData,
    );

    this.kafkaConsumer.registerCallbacksForDeviceMgmtEvents(
      callbackWriteData,
      configDelete['device.data.enable'] ? callbackDeleteMeasurement : null,
    );
  }

  /**
   * Associates callbacks with kafka tenant events to manipulation of influxdb organizations
   */
  initCallbacksTenantToCreateAndDelOrgs() {
    // create callback to handle tenant create
    const boundCreateOrgWithBucket = this.influxOrgs
      .createOrgWithDefaultBucket.bind(this.influxOrgs);
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
    const boundWriterCloseOrg = this.influxWriter
      .closeOne.bind(this.influxWriter);
    const boundDeleteOrgWithBucket = this.influxOrgs
      .deleteOrg.bind(this.influxOrgs);
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

    this.kafkaConsumer.registerCallbackForTenantEvents(
      callbackCreateTenant,
      configDelete['tenant.data.enable'] ? callbackDeleteTenant : null,
    );
  }

  /**
  * Defines the behaver for the shutdown
  */
  defineShutdown() {
    const boundInfluxWriterCloseAll = this.influxWriter
      .closeAll.bind(this.influxWriter);

    const boundKafkaUnregisterCallbacks = this.kafkaConsumer
      .unregisterCallbacks.bind(this.kafkaConsumer);

    const boundKafkaFinish = this.kafkaConsumer
      .finish.bind(this.kafkaConsumer);
    // TODO: Split and register 3 times
    const shutdownFunc = async () => {
      logger.warn('ShutdownHandler: Closing the influxdb-storer...');
      try {
        logger.debug('ShutdownHandler: Trying close all writer...');
        await boundInfluxWriterCloseAll();
        logger.debug('ShutdownHandler: Closed all writer.');

        logger.debug('ShutdownHandler: Trying Unregister Callback for kafka consumer...');
        await boundKafkaUnregisterCallbacks();
        logger.debug('ShutdownHandler: Unregistered callback for kafka consumer.');

        logger.debug('ShutdownHandler: Trying finish kafka consumer...');
        await boundKafkaFinish();
        logger.debug('ShutdownHandler: Finished kafka consumer.');

        logger.info('ShutdownHandler: the service was gracefully shutdown');
      } catch (e) {
        logger.error('ShutdownHandler: Cannot gracefully shutdown...');
        throw e;
      }
    };
    serviceState.registerShutdownHandler(shutdownFunc);
  }

  /**
    * Create a Health Checker  to check if it's possible
    * communication with influxdb
    */
  createHealthCheckers() {
    const boundIsHealthInflux = this.influxState
      .isHealth.bind(this.influxState);

    // TODO: move that to wrapper influx
    const influxdbHealthChecker = async (signalReady, signalNotReady) => {
      const isHealth = await boundIsHealthInflux();
      if (isHealth) {
        logger.debug('influxdbHealthChecker: Server is healthy');
        signalReady();
      } else {
        logger.warn('influxdbHealthChecker: Server is not healthy');
        signalNotReady();
      }
    };
    serviceState.addHealthChecker('influxdb', influxdbHealthChecker, configInflux['heathcheck.ms']);


    const boundIsConnectedKafka = this.kafkaConsumer
      .isConnected.bind(this.kafkaConsumer);
    // TODO: move that to wrapper kafka
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
    serviceState.addHealthChecker('kafka', kafkaHealthChecker, configInflux['heathcheck.ms']);
  }
}

module.exports = App;
