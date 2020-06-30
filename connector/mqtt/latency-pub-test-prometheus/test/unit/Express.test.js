const request = require('supertest');
const latencyStore = require('../../app/LatencyStore');
const ExpressApp = require('../../app/express/App');

let express = null;

jest.mock('../../app/LatencyStore');

describe('loading express', () => {
  beforeEach(() => {
    jest.clearAllMocks();

    latencyStore.getMax
      .mockImplementationOnce(() => 1)
      .mockImplementationOnce(() => null);

    latencyStore.getMin
      .mockImplementationOnce(() => 2)
      .mockImplementationOnce(() => null);

    latencyStore.getAvg
      .mockImplementationOnce(() => 3)
      .mockImplementationOnce(() => null);
    latencyStore.getMedian
      .mockImplementationOnce(() => 4)
      .mockImplementationOnce(() => null);

    latencyStore.getStandardDeviation
      .mockImplementationOnce(() => 5)
      .mockImplementationOnce(() => null);

    latencyStore.getLatencies
      .mockImplementationOnce(() => 10000)
      .mockImplementationOnce(() => null);

    latencyStore.cleanLatencies = jest.fn();
  });

  it('responds to /metrics', async () => {
    express = new ExpressApp();
    express.initListen();

    const response = await request(express.app).get('/metrics');

    expect(response.text).toEqual(
      expect.stringContaining('dojot_latency_pub_statistics{statistic_kind="maximum",unit="ms"} 1'),
      expect.stringContaining('dojot_latency_pub_statistics{ statistic_kind = "minimum", unit = "ms" } 2'),
      expect.stringContaining('dojot_latency_pub_statistics{ statistic_kind = "average", unit = "ms" } 3'),
      expect.stringContaining('dojot_latency_pub_statistics{ statistic_kind = "median", unit = "ms" } 4'),
      expect.stringContaining('dojot_latency_pub_statistics{ statistic_kind = "standardDeviation", unit = "ms" } 5'),
    );
    expect(response.status).toBe(200);

    express.stop();
  });

  it('responds to /metrics with null', async () => {
    express = new ExpressApp();
    express.initListen();

    const response = await request(express.app).get('/metrics');

    expect(response.text).toEqual(
      expect.not.stringContaining('dojot_latency_pub_statistics{statistic_kind="maximum",unit="ms"}'),
      expect.not.stringContaining('dojot_latency_pub_statistics{ statistic_kind = "minimum", unit = "ms" }'),
      expect.not.stringContaining('dojot_latency_pub_statistics{ statistic_kind = "average", unit = "ms" }'),
      expect.not.stringContaining('dojot_latency_pub_statistics{ statistic_kind = "median", unit = "ms" }'),
      expect.not.stringContaining('dojot_latency_pub_statistics{ statistic_kind = "standardDeviation", unit = "ms" }'),
    );
    expect(response.status).toBe(200);

    express.stop();
  });

  it('stop if not init', async () => {
    express = new ExpressApp();

    expect(this.httpServer).not.toBeDefined();

    express.app.isInitialized = false;
    express.stop();

    expect(this.httpServer).not.toBeDefined();
  });
});
