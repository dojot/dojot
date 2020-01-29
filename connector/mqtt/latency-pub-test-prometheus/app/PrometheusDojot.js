const Prometheus = require('prom-client');
const { logger } = require('@dojot/dojot-module-logger');

const TAG = { filename: 'PrometheusDojot' };

const { collectDefaultMetrics } = Prometheus;
collectDefaultMetrics({ prefix: 'dojot_prom_' });


class PrometheusDojot {
  constructor() {
    logger.debug('Init', TAG);

    this.dojotLatency = new Prometheus.Gauge({
      name: 'Dojot_Statistics',
      help: 'Dojot statistics',
      labelNames: ['kind', 'unit'],
    });
  }

  setMax(max) {
    this.dojotLatency.set({ kind: 'max', unit: 'ms' }, max);
  }

  setMin(min) {
    this.dojotLatency.set({ kind: 'min', unit: 'ms' }, min);
  }

  setAvg(avg) {
    this.dojotLatency.set({ kind: 'avg', unit: 'ms' }, avg);
  }

  setMedian(median) {
    this.dojotLatency.set({ kind: 'median', unit: 'ms' }, median);
  }

  setStandardDeviation(standardDeviation) {
    this.dojotLatency.set({ kind: 'standardDeviation', unit: 'ms' }, standardDeviation);
  }
}
const prom = new PrometheusDojot();
module.exports = prom;
