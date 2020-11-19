const {
  ConfigManager: { getConfig },
  Logger,
} = require('@dojot/microservice-sdk');
const InfluxState = require('./State');
const InfluxQuery = require('./Query');

const logger = new Logger('influxdb-retriever:Influx');

const { influx: configInflux } = getConfig('RETRIEVER');

/**
 * Wrapper for InfluxDB
 */
class InfluxDB {
  /**
   * @constructor
   *
   * @param {@dojot/microservice-sdk.ServiceStateManager} serviceState Manages the services' states,
   *                                    providing health check and shutdown utilities.
   */
  constructor(serviceState) {
    this.influxQuery = new InfluxQuery(
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
   *  Returns a Query instance
   * @returns {Query}
   */
  getInfluxQueryInstance() {
    return this.influxQuery;
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
}

module.exports = InfluxDB;
