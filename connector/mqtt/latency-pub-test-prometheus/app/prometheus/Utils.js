const { logger } = require('@dojot/dojot-module-logger');
const Prometheus = require('prom-client');

const TAG = { filename: 'prometheus/Utils' };

/**
 * Some default metrics recommended by Prometheus - Standard and runtime collectors
 */
Prometheus.collectDefaultMetrics({ prefix: 'dojot_latency_pub_' });

/**
 * Get all metrics by a string for prometheus to consume.
 *
 * @return {string} All metrics by a string for prometheus to consume
 */
const getAllRegistersMetricsPrometheus = () => {
  logger.debug('Register Metrics', TAG);

  return Prometheus.register.metrics();
};

/**
 * Reset all metrics.
 */
const resetAllRegistersMetricsPrometheus = () => {
  logger.debug('Reset all metrics', TAG);

  Prometheus.register.resetMetrics();
};


/**
 * ContentType for Prometheus text/plain; version=0.0.4; charset=utf-8
 * @return {string} ContentType
 */
const getRegisterContentTypePrometheus = () => Prometheus.register.contentType;

module.exports = {
  getRegisterContentTypePrometheus,
  getAllRegistersMetricsPrometheus,
  resetAllRegistersMetricsPrometheus,
};
