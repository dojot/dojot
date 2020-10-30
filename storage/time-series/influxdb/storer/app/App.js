const flatten = require('flat');
const { ConfigManager, Logger } = require('@dojot/microservice-sdk');
const camelCase = require('lodash.camelcase');
const ServiceState = require('./ServiceStateMgmt');
const InfluxState = require('./influx/State');
const KafkaConsumer = require('./kafka/DojotConsumer');
const InfluxWriter = require('./influx/WriterData');
const InfluxOrgs = require('./influx/Organizations');
const InfluxMeasurement = require('./influx/Measurements');

const { influx: configInflux, delete: configDelete } = ConfigManager.getConfig('STORER');

const { write: { options: influxWriteOptions } } = flatten.unflatten(configInflux);
const configInfluxWriteOptions = influxWriteOptions ? flatten(influxWriteOptions) : {};
const configInfluxWriteOptionsCamelCase = ConfigManager
  .transformObjectKeys(configInfluxWriteOptions, camelCase);

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
      this.createHealthCheckers();
      this.defineShutdown();

      // initializes kafka consumer
      await this.kafkaConsumer.init();

      const kafkaIsConnected = await this.kafkaConsumer.isConnected();
      if (!kafkaIsConnected) {
        throw new Error('Kafka is not ready');
      }
      const influxIsReady = await this.influxState.isReady();
      if (!influxIsReady) {
        throw new Error('Influxdb is not ready');
      }

      // create the initial org, user and bucket
      await this.influxOrgs.initOnboarding();

      // associate by callback kafka consumers and influxdb actions
      this.initCallbacksTenantToCreateAndDelOrgs();
      this.initCallbacksDevicesCreateAndDelData();

      // Signals to the health check service that the service is ready.
      logger.debug('Init: Signaling that the service is ready...');
      ServiceState.signalReady();
      logger.debug('Init: ...signaled.');
    } catch (e) {
      logger.error('init:', e);
      await ServiceState.shutdown();
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
      try {
        await boundInfluxWriter(tenant, deviceid, attrs, timestamp);
      } catch (e) {
        logger.error('callbackWriteData:', e);
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
        logger.error('callbackDeleteMeasurement:', e);
      }
    };

    this.kafkaConsumer.registerCallbackDeviceData(
      callbackWriteData,
    );

    this.kafkaConsumer.registerCallbackDeviceManager(
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
        logger.error('callbackCreateTenant:', e);
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
        logger.error('callbackDeleteTenant:', e);
      }
    };

    this.kafkaConsumer.registerCallbackTenants(
      callbackCreateTenant,
      configDelete['tenant.data.enable'] ? callbackDeleteTenant : null,
    );
  }

  /**
  * Defines the behaver for the shutdown
  */
  defineShutdown() {
    const boundWriterCloseAll = this.influxWriter
      .closeAll.bind(this.influxWriter);

    const boundUnregisterCallbacks = this.kafkaConsumer
      .unregisterCallbacks.bind(this.kafkaConsumer);
    const shutdownFunc = async () => {
      logger.warn('ShutdownHandler: Closing the influxdb-storer...');
      try {
        logger.debug('ShutdownHandler: Trying close all writer...');
        await boundWriterCloseAll();
        logger.debug('ShutdownHandler: Closed all writer.');

        logger.debug('ShutdownHandler: Trying Unregister Callback for kafka consumer...');
        await boundUnregisterCallbacks();
        logger.debug('ShutdownHandler: Unregistered callback for kafka consumer.');

        logger.info('ShutdownHandler: the service was gracefully shutdown');
      } catch (e) {
        logger.error('ShutdownHandler: Cannot gracefully shutdown...');
        throw e;
      }
    };
    ServiceState.registerShutdown(shutdownFunc);
  }

  /**
    * Create a Health Checker  to check if it's possible
    * communication with influxdb
    */
  createHealthCheckers() {
    const boundIsHealthInflux = this.influxState
      .isHealth.bind(this.influxState);

    const boundIsConnectedKafka = this.kafkaConsumer
      .isConnected.bind(this.kafkaConsumer);

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
    ServiceState.addHealthChecker(influxdbHealthChecker, configInflux['heathcheck.ms']);

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
    ServiceState.addHealthChecker(kafkaHealthChecker, configInflux['heathcheck.ms']);
  }
}

module.exports = App;
