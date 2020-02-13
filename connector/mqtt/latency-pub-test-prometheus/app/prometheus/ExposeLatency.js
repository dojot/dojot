const Prometheus = require('prom-client');
const { logger } = require('@dojot/dojot-module-logger');

const TAG = { filename: 'prometheus/ExposeLatency' };

/**
 * Class to Expose latency to Prometheus, latencies between publication MQTT and kafka with Dojot
 */
class ExposeLatency {
  /**
   * Init Prometheus Gauge
   */
  constructor() {
    logger.debug('Init Export Latency for Prometheus ...', TAG);

    this.latency = new Prometheus.Gauge({
      name: 'dojot_latency_pub_statistics',
      help: 'Latency between publication MQTT and Kafka with Prometheus and Dojot',
      labelNames: ['statistic_kind', 'unit'],
    });
  }

  /**
   * Set current maximum latency
   *
   * @param {number} max Maximum latency in ms
   */
  setMax(max) {
    if (max !== null) {
      this.latency.set({ statistic_kind: 'maximum', unit: 'ms' }, max);
    }
  }


  /**
   * Set current minimum latency
   *
   * @param {number} min Minimum latency in ms
   */
  setMin(min) {
    if (min !== null) {
      this.latency.set({ statistic_kind: 'minimum', unit: 'ms' }, min);
    }
  }

  /**
   * Set current average latency
   *
   * @param {number} avg Average latency in ms
   */
  setAvg(avg) {
    if (avg !== null) {
      this.latency.set({ statistic_kind: 'average', unit: 'ms' }, avg);
    }
  }

  /**
   * Set current median latency
   *
   * @param {number} median Median latency in ms
   */
  setMedian(median) {
    if (median !== null) {
      this.latency.set({ statistic_kind: 'median', unit: 'ms' }, median);
    }
  }

  /**
   * Set current Standard Deviation latency
   *
   * @param {number} standardDeviation  Standard Deviation latency in ms
   */
  setStandardDeviation(standardDeviation) {
    if (standardDeviation !== null) {
      this.latency.set({ statistic_kind: 'standard_deviation', unit: 'ms' }, standardDeviation);
    }
  }
}

const prom = new ExposeLatency();
module.exports = prom;
