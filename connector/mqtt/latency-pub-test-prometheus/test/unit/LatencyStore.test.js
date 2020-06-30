const metrics = require('../../app/LatencyStore');

jest.mock('@dojot/dojot-module-logger');
jest.mock('simple-statistics');

jest.mock('simple-statistics', () => ({
  mean: jest.fn(() => 3),
  median: jest.fn(() => 4),
  standardDeviation: jest.fn(() => 5),
}));

metrics.cleanLatencies();

describe('Testing Latency Store', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('Add many and get statistics', () => {
    const times = [100, 120, 130, 140, 90];

    times.forEach((time) => {
      metrics.addLatency(time);
    });

    expect(metrics.getLatencies()).toStrictEqual(times);

    expect(metrics.getMax()).toBe(140);
    expect(metrics.getMin()).toBe(90);
    expect(metrics.getAvg()).toBe(3);
    expect(metrics.getMedian()).toBe(4);
    expect(metrics.getStandardDeviation()).toBe(5);
  });

  it('Gets statistics All Clean', () => {
    metrics.cleanLatencies();

    expect(metrics.getMax()).toBe(null);
    expect(metrics.getMin()).toBe(null);
    expect(metrics.getAvg()).toBe(null);
    expect(metrics.getMedian()).toBe(null);
    expect(metrics.getStandardDeviation()).toBe(null);
  });
});
