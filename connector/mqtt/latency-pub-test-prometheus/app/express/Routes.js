const routes = require('express').Router();
const { logger } = require('@dojot/dojot-module-logger');
const util = require('util');

const ExposeLatency = require('../prometheus/ExposeLatency');

const {
  getRegisterContentTypePrometheus,
  getAllRegistersMetricsPrometheus,
  resetAllRegistersMetricsPrometheus,
} = require('../prometheus/Utils');

const latencyStore = require('../LatencyStore');

const TAG = { filename: 'express/Routes' };

/**
 * Endpoint  for prometheus to consume metrics
 */
routes.get('/metrics', (req, res) => {
  // set content type used for prometheus
  res.set('Content-Type', getRegisterContentTypePrometheus());

  // get store latencies and set in prometheus metrics
  ExposeLatency.setMax(latencyStore.getMax());
  ExposeLatency.setMin(latencyStore.getMin());
  ExposeLatency.setAvg(latencyStore.getAvg());
  ExposeLatency.setMedian(latencyStore.getMedian());
  ExposeLatency.setStandardDeviation(latencyStore.getStandardDeviation());

  logger.debug(`Expose metrics ${util.inspect(latencyStore.getLatencies(), { depth: null })}`, TAG);

  // clean store latencies
  latencyStore.cleanLatencies();

  // pass a string to be sent to the client
  res.end(getAllRegistersMetricsPrometheus());

  logger.debug(`Expose metrics to prometheus ${getAllRegistersMetricsPrometheus()}`, TAG);

  resetAllRegistersMetricsPrometheus();
});

module.exports = routes;
