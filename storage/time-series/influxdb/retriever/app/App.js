const {
  ServiceStateManager,
  ConfigManager: { getConfig, transformObjectKeys },
} = require('@dojot/microservice-sdk');

const path = require('path');

const camelCase = require('lodash.camelcase');

const {
  lightship: configLightship,
} = getConfig('RETRIEVER');

const serviceState = new ServiceStateManager({
  lightship: transformObjectKeys(configLightship, camelCase),
});
serviceState.registerService('server');
serviceState.registerService('influxdb');

const Server = require('./Server');
const express = require('./express');
const devicesRoutes = require('./express/routes/v1/Devices');

const InfluxState = require('./influx/State');


const openApiPath = path.join(__dirname, '../api/v1.yml');

/**
* Wrapper to initialize the service
*/
class App {
  /**
    * Constructor App
    * that instantiate influxdb classes
    */
  constructor(dependencyContainer, logger) {
    logger.debug('constructor: instantiate app...');
    try {
      this.logger = logger;
      this.server = new Server(serviceState);
      this.influxState = new InfluxState(dependencyContainer.influxDBConnection, serviceState);
      this.localPersistence = dependencyContainer.localPersistence;
      this.deviceDataService = dependencyContainer.deviceDataService;
      this.genericQueryService = dependencyContainer.genericQueryService;
      this.deviceDataRepository = dependencyContainer.deviceDataRepository;
      this.tenantService = dependencyContainer.tenantService;
      this.deviceManagerService = dependencyContainer.deviceManagerService;
      this.syncLoader = dependencyContainer.syncLoader;
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
    this.logger.info('init: Initializing the influxdb-retriever...');
    try {
      const influxIsReady = await this.influxState.isReady();
      if (!influxIsReady) {
        throw new Error('Influxdb is not ready');
      }
      this.influxState.createInfluxHealthChecker();
      this.server.registerShutdown();
      await this.syncLoader.init();

      // Initializes API
      this.server.init(express(
        [
          devicesRoutes({
            deviceManagerService: this.deviceManagerService,
            mountPoint: '/tss/v1',
            deviceDataService: this.deviceDataService,
            genericQueryService: this.genericQueryService,
            deviceDataRepository: this.deviceDataRepository,
          }),
        ],
        serviceState,
        openApiPath,
        this.tenantService,
      ));
    } catch (e) {
      this.logger.error('init:', e);
      throw e;
    }
    this.logger.info('init:...service initialized.');
  }
}

module.exports = App;
