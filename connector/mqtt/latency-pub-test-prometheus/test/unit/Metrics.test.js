const metrics = require('../../app/Metrics');

jest.mock('@dojot/dojot-module-logger');
jest.mock('simple-statistics');


jest.mock('simple-statistics', () => ({
  max: jest.fn(() => 1),
  min: jest.fn(() => 2),
  mean: jest.fn(() => 3),
  median: jest.fn(() => 4),
  standardDeviation: jest.fn(() => 5),
}));


metrics.cleanAllTimes();

describe('Testing Metrics', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('Add many ', () => {
    const times = [100, 120, 130, 140];
    times.forEach((time) => {
      metrics.addTime(time);
    });

    expect(metrics.getAllTimes()).toStrictEqual(times);
  });

  it('Gets statistics', () => {
    expect(metrics.getMax()).toBe(1);
    expect(metrics.getMin()).toBe(2);
    expect(metrics.getAvg()).toBe(3);
    expect(metrics.getMedian()).toBe(4);
    expect(metrics.getStandardDeviation()).toBe(5);
  });

  it('Gets statistics All Clean', () => {
    metrics.cleanAllTimes();
    expect(metrics.getMax()).toBe(0);
    expect(metrics.getMin()).toBe(0);
    expect(metrics.getAvg()).toBe(0);
    expect(metrics.getMedian()).toBe(0);
    expect(metrics.getStandardDeviation()).toBe(0);
  });
});
