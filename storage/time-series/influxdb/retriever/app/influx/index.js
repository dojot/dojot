const {
  ConfigManager: { getConfig },
  Logger,
} = require('@dojot/microservice-sdk');
const InfluxState = require('./State');
const InfluxDataQuery = require('./DataQuery');

const logger = new Logger('influxdb-retriever:Influx');

const { influx: configInflux } = getConfig('RETRIEVER');

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
    this.influxDataQuery = new InfluxDataQuery(
      configInflux.url,
      configInflux['default.token'],
      configInflux['default.bucket'],
    );
    this.influxState = new InfluxState(
      configInflux.url,
    );
    this.serviceState = serviceState;
  }

  /**
   *  Returns a Query instance
   * @returns {Query}
   */
  getInfluxDataQueryInstance() {
    return this.influxDataQuery;
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
  createInfluxHealthChecker() {
    const influxdbHealthChecker = async (signalReady, signalNotReady) => {
      const isHealth = await this.influxState.isHealth();
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
}

module.exports = InfluxDB;
