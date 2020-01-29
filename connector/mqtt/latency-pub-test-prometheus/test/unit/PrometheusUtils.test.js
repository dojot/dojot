const Prometheus = require('prom-client');
const { getRegisterMetrics, getRegisterContentType } = require('../../app/PrometheusUtils');

jest.mock('@dojot/dojot-module-logger');
jest.mock('prom-client');


describe('Testing Metrics', () => {
  it('Check has call RegisterMetrics ', () => {
    getRegisterMetrics();

    expect(Prometheus.register.metrics).toHaveBeenCalled();
  });

  it('Check has call RegisterContentType ', () => {
    Prometheus.register.contentType = '';

    const contentType = getRegisterContentType();

    expect(contentType).toBeDefined();
  });
});
