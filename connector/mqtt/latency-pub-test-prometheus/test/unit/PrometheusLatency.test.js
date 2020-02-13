const Prometheus = require('prom-client');
const prometheusDojot = require('../../app/prometheus/ExposeLatency');

jest.mock('@dojot/dojot-module-logger');
jest.mock('prom-client');

describe('Testing Metrics', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    Prometheus.collectDefaultMetrics = (jest.fn(() => ''));
  });

  it('Check instance ', () => {
    expect(prometheusDojot.latency).toBeDefined();
    expect(prometheusDojot.latency instanceof Prometheus.Gauge).toBeTruthy();
  });

  it('Check Sets ', () => {
    prometheusDojot.setMax(555);
    prometheusDojot.setAvg(555);
    prometheusDojot.setMedian(555);
    prometheusDojot.setMin(555);
    prometheusDojot.setStandardDeviation(555);

    expect(prometheusDojot.latency.set).toHaveBeenCalledTimes(5);
  });
});
