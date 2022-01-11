const {
  ServiceStateManager,
  ConfigManager: { getConfig, transformObjectKeys },
  Logger,
  LocalPersistence: {
    LocalPersistenceManager,
  },
} = require('@dojot/microservice-sdk');

const { InfluxDB } = require('@influxdata/influxdb-client');

const path = require('path');

const camelCase = require('lodash.camelcase');

const { lightship: configLightship, sync, influx: configInflux } = getConfig('RETRIEVER');

const serviceState = new ServiceStateManager({
  lightship: transformObjectKeys(configLightship, camelCase),
});
serviceState.registerService('server');
serviceState.registerService('influxdb');

const logger = new Logger('influxdb-retriever:App');


const Server = require('./Server');
const RetrieverConsumer = require('./sync/RetrieverConsumer');

const express = require('./express');
const devicesRoutes = require('./express/routes/v1/Devices');
const DeviceManagerService = require('./sync/DeviceManagerService');
const SyncLoader = require('./sync/SyncLoader');
const TenantService = require('./sync/TenantService');
const DeviceDataRepository = require('./influx/DeviceDataRepository');
const DeviceDataService = require('./express/services/v1/DeviceDataService');
const InfluxState = require('./influx/State');
const GenericQueryService = require('./express/services/v1/GenericQueryService');

const openApiPath = path.join(__dirname, '../api/v1.yml');

/**
* Wrapper to initialize the service
*/
class App {
  /**
    * Constructor App
    * that instantiate influxdb classes
    */
  constructor() {
    logger.debug('constructor: instantiate app...');
    try {
      // Extern dependencies
      this.server = new Server(serviceState);
      this.influxDBConnection = new InfluxDB({
        url: configInflux.url,
        token: configInflux['default.token'],
        timeout: configInflux['max.timeout.ms'],
      });
      this.influxState = new InfluxState(this.influxDBConnection, serviceState);

      // Intern dependencies
      // API Dependencies
      this.deviceDataRepository = new DeviceDataRepository(configInflux['default.bucket'], this.influxDBConnection);
      this.deviceDataService = new DeviceDataService(this.deviceDataRepository);
      this.genericQueryService = new GenericQueryService(this.deviceDataRepository);
      // Sync Dependencies
      this.localPersistence = new LocalPersistenceManager(
        logger, true, sync['database.path'],
      );
      this.retrieverConsumer = new RetrieverConsumer(this.localPersistence);
      this.authService = new TenantService(sync.tenants);
      this.deviceManagerService = new DeviceManagerService(sync.devices);
      this.syncLoader = new SyncLoader(
        this.localPersistence, this.authService, this.deviceManagerService, this.retrieverConsumer,
      );
    } catch (e) {
      logger.error('constructor:', e);
      const er = new Error(e);
      er.message = `constructor: ${e.message}`;
      throw er;
    }
  }

  /**
   * Initialize the server and influxdb
   */
  async init() {
    logger.info('init: Initializing the influxdb-retriever...');
    try {
      const influxIsReady = await this.influxState.isReady();
      if (!influxIsReady) {
        throw new Error('Influxdb is not ready');
      }
      this.influxState.createInfluxHealthChecker();
      this.server.registerShutdown();
      this.syncLoader.init();

      // Initializes API
      this.server.init(express(
        [
          devicesRoutes({
            localPersistence: this.localPersistence,
            mountPoint: '/tss/v1',
            deviceDataService: this.deviceDataService,
            genericQueryService: this.genericQueryService,
            deviceDataRepository: this.deviceDataRepository,
          }),
        ],
        serviceState,
        openApiPath,
      ));
    } catch (e) {
      logger.error('init:', e);
      throw e;
    }
    logger.info('init:...service initialized.');
  }
}

module.exports = App;
