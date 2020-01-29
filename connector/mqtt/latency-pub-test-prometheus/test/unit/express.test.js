const request = require('supertest');

let express = null;

jest.mock('../../app/Metrics');

jest.mock('../../app/Metrics', () => ({
  getMax: jest.fn(() => 1),
  getMin: jest.fn(() => 2),
  getAvg: jest.fn(() => 3),
  getMedian: jest.fn(() => 4),
  getStandardDeviation: jest.fn(() => 5),
  getAllTimes: jest.fn(() => 10000),
  cleanAllTimes: jest.fn(),
}));
const ExpressApp = require('../../app/express/App');

describe('loading express', () => {
  it('responds to /metrics', async () => {
    express = new ExpressApp();
    express.initListen();

    const response = await request(express.app).get('/metrics');

    expect(response.text).toEqual(
      expect.stringContaining('Dojot_Statistics{kind="max",unit="ms"} 1'),
      expect.stringContaining('Dojot_Statistics{ kind = "min", unit = "ms" } 2'),
      expect.stringContaining('Dojot_Statistics{ kind = "avg", unit = "ms" } 3'),
      expect.stringContaining('Dojot_Statistics{ kind = "median", unit = "ms" } 4'),
      expect.stringContaining('Dojot_Statistics{ kind = "standardDeviation", unit = "ms" } 5'),
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
