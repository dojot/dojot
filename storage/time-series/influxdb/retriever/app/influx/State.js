const { HealthAPI, ReadyAPI } = require('@influxdata/influxdb-client-apis');
const { Logger, ConfigManager: { getConfig } } = require('@dojot/microservice-sdk');


const logger = new Logger('influxdb-retriever:influx/State');
const { influx: configInflux } = getConfig('RETRIEVER');

/**
 * This class handles with State (ready and heath) from InfluxDB
 * @class
 */
class InfluxState {
  /**
   *
   * @param {String} url Url to access influxdb
   */
  constructor(influxDB, serviceState) {
    this.serviceState = serviceState;
    this.healthAPI = new HealthAPI(influxDB);
    this.readyAPI = new ReadyAPI(influxDB);
  }

  /**
   * Checks if influxdb is Health
   * @returns {Promise.<boolean>} true if exist or false otherwise
   *
   */
  async isHealth() {
    try {
      const result = await this.healthAPI.getHealth();
      logger.debug(`isHealth: ${result.status === 'pass' ? 'OK' : 'NOT OK'}`, result);
      return result.status === 'pass';
    } catch (e) {
      logger.error('isHealth:', e);
      return false;
    }
  }

  /**
   * Checks if influxdb is Ready
   * @returns {Promise.<boolean>} true if exist or false otherwise
   *
   */
  async isReady() {
    try {
      const result = await this.readyAPI.getReady();
      logger.debug(`isReady: ${result.status === 'ready' ? 'OK' : 'NOT OK'}`, result);
      return result.status === 'ready';
    } catch (e) {
      logger.error('isReady:', e);
      return false;
    }
  }

  /**
   * Create a 'healthCheck' for influxDB
   */
  createInfluxHealthChecker() {
    const influxdbHealthChecker = async (signalReady, signalNotReady) => {
      const isHealth = await this.isHealth();
      if (isHealth) {
        logger.debug('influxdbHealthChecker: InfluxDB is healthy');
        signalReady();
      } else {
        logger.warn('influxdbHealthChecker: InfluxDB is not healthy');
        signalNotReady();
      }
    };
    this.serviceState.addHealthChecker(
      'influxdb', influxdbHealthChecker, configInflux['heathcheck.ms'],
    );
  }
}
module.exports = InfluxState;
