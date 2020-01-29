const { logger } = require('@dojot/dojot-module-logger');

const TAG = { filename: 'PrometheusUtils' };

const Prometheus = require('prom-client');

const getRegisterMetrics = () => {
  logger.debug('Register Metrics', TAG);

  return Prometheus.register.metrics();
};

const getRegisterContentType = () => Prometheus.register.contentType;

module.exports = {
  getRegisterMetrics,
  getRegisterContentType,
};
