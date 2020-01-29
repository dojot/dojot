const Prometheus = require('prom-client');
const prometheusDojot = require('../../app/PrometheusDojot');

jest.mock('@dojot/dojot-module-logger');
jest.mock('prom-client');


describe('Testing Metrics', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('Check instance ', () => {
    expect(prometheusDojot.dojotLatency).toBeDefined();
    expect(prometheusDojot.dojotLatency instanceof Prometheus.Gauge).toBeTruthy();
  });

  it('Check Sets ', () => {
    prometheusDojot.setMax(555);
    prometheusDojot.setAvg(555);
    prometheusDojot.setMedian(555);
    prometheusDojot.setMin(555);
    prometheusDojot.setStandardDeviation(555);

    expect(prometheusDojot.dojotLatency.set).toHaveBeenCalledTimes(5);
  });
});
