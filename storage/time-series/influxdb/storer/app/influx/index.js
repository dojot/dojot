const {
  ConfigManager: { getConfig, transformObjectKeys },
  Logger,
} = require('@dojot/microservice-sdk');
const camelCase = require('lodash.camelcase');
const flatten = require('flat');
const InfluxState = require('./State');
const InfluxDataWriter = require('./DataWriter');
const InfluxOrgs = require('./Organizations');
const InfluxMeasurement = require('./Measurements');

const logger = new Logger('influxdb-storer:Influx');
const { influx: configInflux } = getConfig('STORER');

const { write: { options: influxWriteOptions } } = flatten.unflatten(configInflux);
const configInfluxWriteOptions = influxWriteOptions ? flatten(influxWriteOptions) : {};
const configInfluxWriteOptionsCamelCase = transformObjectKeys(configInfluxWriteOptions, camelCase);


/**
 * Wrapper for InfluxDB
 */
class InfluxDB {
  /**
     * @constructor
     *
     * @param {@dojot/microservice-sdk
     *                  .ServiceStateManager} serviceState Manages the services' states,
     *                                    providing health check and shutdown utilities.
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
    this.serviceState.registerService('influxdb');
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
     * Create a 'healthCheck' for influxDB
     */
  createHealthChecker() {
    const boundIsHealthInflux = this.influxState
      .isHealth.bind(this.influxState);

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
    this.serviceState.addHealthChecker('influxdb', influxdbHealthChecker, configInflux['heathcheck.ms']);
  }


  /**
   *  Register a shutdown to the http server
   */
  async registerShutdown() {
    const boundInfluxWriterCloseAll = this.influxDataWriter
      .closeAll.bind(this.influxDataWriter);
    this.serviceState.registerShutdownHandler(async () => {
      logger.debug('ShutdownHandler: Trying close all writer...');
      await boundInfluxWriterCloseAll();
      logger.warn('ShutdownHandler: Closed all writer.');
    });
  }
}

module.exports = InfluxDB;
