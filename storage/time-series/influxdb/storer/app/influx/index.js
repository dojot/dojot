const {
  ConfigManager: { getConfig, transformObjectKeys },
  Logger,
} = require('@dojot/microservice-sdk');
const camelCase = require('lodash.camelcase');
const { flatten, unflatten } = require('flat');
const InfluxState = require('./State');
const InfluxDataWriter = require('./DataWriter');
const InfluxOrgs = require('./Organizations');
const InfluxMeasurement = require('./Measurements');

const logger = new Logger('influxdb-storer:Influx');
const { influx: configInflux } = getConfig('STORER');
let configInfluxWriteOptionsCamelCase = {};

if (configInflux && typeof configInflux === 'object') {
  const { write: { options: influxWriteOptions } } = unflatten(configInflux);
  const configInfluxWriteOptions = influxWriteOptions ? flatten(influxWriteOptions) : {};
  configInfluxWriteOptionsCamelCase = transformObjectKeys(configInfluxWriteOptions, camelCase);
}

/**
 * Wrapper for InfluxDB
 */
class InfluxDB {
  /**
 * @constructor
 *
 * @param {an instance of @dojot/microservice-sdk.ServiceStateManager
 *          with register service 'influxdb'} serviceState
 *          Manages the services' states, providing health check and shutdown utilities.
*/
  constructor(serviceState) {
    this.influxOrgs = new InfluxOrgs(
      configInflux.url,
      configInflux['default.token'],
      configInflux['default.user'],
      configInflux['default.password'],
      configInflux['default.organization'],
      configInflux['default.bucket'],
      configInflux['retention.hrs'],
    );
    this.influxDataWriter = new InfluxDataWriter(
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
    this.serviceState = serviceState;
    this.isHealth = null;
  }

  /**
   *  Returns a Org instance
   * @returns {Organizations}
   */
  getInfluxOrgInstance() {
    return this.influxOrgs;
  }

  /**
 *  Returns a Writer instance
 * @returns {Writer}
 */
  getInfluxDataWriterInstance() {
    return this.influxDataWriter;
  }

  /**
   *  Returns a Measurement instance
   * @returns {Measurement}
   */
  getInfluxMeasurementInstance() {
    return this.influxMeasurement;
  }

  /**
   *  Returns a State instance
   * @returns {State}
   */
  getInfluxStateInstance() {
    return this.influxState;
  }

  /**
   * Creates a 'healthCheck' for influxDB
   *
   * @param {function()} cbHealth Callback to be called if the service is
   *                              healthy, this.isHealth is not null
   *                              and if the value of this.isHealth is to change.
   * @param {function()} cbNotHealth Callback to be called if the service is
   *                                 not healthy,  this.isHealth is not null
   *                                 and if the value of this.isHealth is to change.
   */
  createHealthChecker(cbHealth, cbNotHealth) {
    const influxdbHealthChecker = async (signalReady, signalNotReady) => {
      const isHealth = await this.influxState.isHealth();
      if (isHealth) {
        logger.debug('createHealthChecker: InfluxDB is healthy');
        if (this.isHealth !== null && !this.isHealth) {
          logger.debug('createHealthChecker: Calling callback health');
          cbHealth();
        }
        this.isHealth = true;
        signalReady();
      } else {
        logger.warn('createHealthChecker: InfluxDB is not healthy');
        if (this.isHealth !== null && this.isHealth) {
          logger.warn('createHealthChecker: Calling callback not health');
          cbNotHealth();
        }
        this.isHealth = false;
        signalNotReady();
      }
    };
    this.serviceState.addHealthChecker('influxdb', influxdbHealthChecker, configInflux['heathcheck.ms']);
  }


  /**
   *  Registers a shutdown to the http server
   */
  async registerShutdown() {
    this.serviceState.registerShutdownHandler(async () => {
      logger.debug('ShutdownHandler: Trying close all writer...');
      await this.influxDataWriter.closeAll();
      logger.warn('ShutdownHandler: Closed all writer.');
    });
  }
}

module.exports = InfluxDB;
