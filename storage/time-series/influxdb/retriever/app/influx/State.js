const { InfluxDB } = require('@influxdata/influxdb-client');
const { HealthAPI, ReadyAPI } = require('@influxdata/influxdb-client-apis');
const { Logger } = require('@dojot/microservice-sdk');


const logger = new Logger('influxdb-retriever:influx/State');

/**
 * This class handles with State (ready and heath) from InfluxDB
 * @class
 */
class State {
  /**
   *
   * @param {String} url Url to access influxdb
   */
  constructor(url) {
    logger.debug('constructor:');
    logger.debug(`constructor: url=${url}`);
    const influxDB = new InfluxDB({ url });
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
}
module.exports = State;
