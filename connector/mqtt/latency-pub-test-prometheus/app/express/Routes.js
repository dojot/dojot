const routes = require('express').Router();
const { logger } = require('@dojot/dojot-module-logger');
const util = require('util');

const PrometheusDojot = require('../PrometheusDojot');
const { getRegisterContentType, getRegisterMetrics } = require('../PrometheusUtils');
const metrics = require('../Metrics');

const TAG = { filename: 'express/Routes' };

routes.get('/metrics', (req, res) => {
  res.set('Content-Type', getRegisterContentType());

  PrometheusDojot.setMax(metrics.getMax());
  PrometheusDojot.setMin(metrics.getMin());
  PrometheusDojot.setAvg(metrics.getAvg());
  PrometheusDojot.setMedian(metrics.getMedian());
  PrometheusDojot.setStandardDeviation(metrics.getStandardDeviation());

  logger.debug(`Expose metrics ${util.inspect(metrics.getAllTimes(), { depth: null })}`, TAG);

  metrics.cleanAllTimes();

  res.end(getRegisterMetrics());
});

module.exports = routes;
