const ss = require('simple-statistics');
const { logger } = require('@dojot/dojot-module-logger');

const TAG = { filename: 'LatencyStore' };

/* * *
 * Class to store latencies and calculate statistics
 * Export the instance of the class,
 * that restricts the instantiation of a class to one "single" instance.
 * * */
class LatencyStore {
  /**
  * Cannot construct outside this file
  */
  constructor() {
    logger.debug('Init LatencyStore...', TAG);

    this.latencies = [];
    this.max = null;
    this.min = null;
  }

  /**
     * Retrieve array of latencies
     *
     * @return {Array} All latencies
     */
  getLatencies() {
    return this.latencies;
  }

  /**
     * Clean all store info
     */
  cleanLatencies() {
    logger.debug(`Clean all Latencies, the Latencies before clean: ${this.getLatencies()}`, TAG);

    this.latencies = [];
    this.max = null;
    this.min = null;
  }

  /**
     * Add a new latency to store
     *
     * @param {number} latency in ms
     */
  addLatency(latency) {
    this.latencies.push(latency);

    this.updateMax(latency);
    this.updateMin(latency);
  }

  /**
     * Update the attribute of classe min if param is less then current
     *
     * @param {number} Latency
     */
  updateMin(latency) {
    this.min = this.min === null || this.min > latency ? latency : this.min;
  }

  /**
     * Update the attribute of classe max if param is more then current
     *
     * @param {number} Latency
     */
  updateMax(latency) {
    this.max = this.max === null || this.max < latency ? latency : this.max;
  }

  /**
     * Get maximum value for all latencies stores
     * @return {number} Maximum for all latencies stores
     */
  getMax() {
    return this.max;
  }

  /**
     * Get minimum value for all latencies stores
     * @return {number} minimum for all latencies stores
     */
  getMin() {
    return this.min;
  }

  /**
    * Get average value for all latencies stores
    * @return {number} Average for all latencies stores
    */
  getAvg() {
    return this.getLatencies().length > 0 ? ss.mean(this.getLatencies()) : null;
  }

  /**
    * Get median value for all latencies stores
    * @return {number} Median for all latencies stores
    */
  getMedian() {
    return this.getLatencies().length > 0 ? ss.median(this.getLatencies()) : null;
  }

  /**
    * Get Standard Deviation value for all latencies stores
    * @return {number} Standard Deviation for all latencies stores
    */
  getStandardDeviation() {
    return this.getLatencies().length > 0 ? ss.standardDeviation(this.getLatencies()) : null;
  }
}

const latencyStore = new LatencyStore();
module.exports = latencyStore;
