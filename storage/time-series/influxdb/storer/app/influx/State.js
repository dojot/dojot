const { InfluxDB } = require('@influxdata/influxdb-client');
const { HealthAPI, ReadyAPI } = require('@influxdata/influxdb-client-apis');
const { Logger } = require('@dojot/microservice-sdk');


const logger = new Logger('influxdb:influx/State');

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
   * Check if influxdb is Heath
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
   * Check if influxdb is Ready
   * @returns {Promise.<boolean>} true if exist or false otherwise
   *
   */
  async isReady() {
    try {
      const result = await this.readyAPI.getReady();
      // ATTENTION:  It's expected a JavaScript object, but is returned a serialized object see https://github.com/influxdata/influxdb-client-js/issues/277
      const resultObj = JSON.parse(result);
      logger.debug(`isReady: ${resultObj.status === 'ready' ? 'OK' : 'NOT OK'}`, resultObj);
      return resultObj.status === 'ready';
    } catch (e) {
      logger.error('isReady:', e);
      return false;
    }
  }
}
module.exports = State;
