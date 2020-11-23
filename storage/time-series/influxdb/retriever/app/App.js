const {
  ServiceStateManager,
  ConfigManager: { getConfig, transformObjectKeys },
  Logger,
} = require('@dojot/microservice-sdk');
const path = require('path');

const camelCase = require('lodash.camelcase');

const { lightship: configLightship } = getConfig('RETRIEVER');

const serviceState = new ServiceStateManager({
  lightship: transformObjectKeys(configLightship, camelCase),
});

const logger = new Logger('influxdb-retriever:App');

const Server = require('./Server');
const InfluxDB = require('./influx');

const express = require('./express');
const devicesRoutes = require('./express/routes/v1/Devices');

const openApiPath = path.join(__dirname, '../api/swagger.yml');


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
      this.server = new Server(serviceState);
      this.influxDB = new InfluxDB(serviceState);
    } catch (e) {
      logger.error('constructor:', e);
      throw e;
    }
  }

  /**
   * Initialize the server and influxdb
   */
  async init() {
    logger.info('init: Initializing the influxdb-retriever...');
    try {
      const influxIsReady = await this.influxDB.getInfluxStateInstance().isReady();
      if (!influxIsReady) {
        throw new Error('Influxdb is not ready');
      }

      this.influxDB.createInfluxHealthChecker();

      const boundQueryData = this
        .influxDB.getInfluxQueryInstance()
        .queryDataByField.bind(this.influxDB.getInfluxQueryInstance());

      this.server.registerShutdown();

      this.server.init(express(
        [
          devicesRoutes({
            mountPoint: '/tss/v1',
            queryData: boundQueryData,
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
