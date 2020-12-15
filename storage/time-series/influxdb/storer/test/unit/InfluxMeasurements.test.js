
const mockWarn = jest.fn();
const mockSdk = {
  Logger: jest.fn(() => ({
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    warn: mockWarn,

  })),
};
jest.mock('@dojot/microservice-sdk', () => mockSdk);

const mockInflux = {
  InfluxDB: jest.fn().mockImplementation(),
};
jest.mock('@influxdata/influxdb-client', () => mockInflux);

const mockPostDelete = jest.fn();
const mockInfluxApi = {
  DeleteAPI: jest.fn().mockImplementation(() => ({
    postDelete: mockPostDelete,
  })),
};
jest.mock('@influxdata/influxdb-client-apis', () => mockInfluxApi);


const Measurements = require('../../app/influx/Measurements');

describe('Test Influx Measurements', () => {
  let measur = null;
  beforeAll(() => {
    measur = null;
  });

  beforeEach(() => {
    jest.clearAllMocks();
  });

  afterAll(() => {
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  test('Instantiate class', () => {
    measur = new Measurements('url', 'token', 'defaultBucket');
  });

  test('remove - test ok ', async () => {
    const mockDate = new Date(1666434490000);
    const spy = jest
      .spyOn(global, 'Date')
      .mockImplementation(() => mockDate);
    await measur.deleteMeasurement('org', 'measurement');
    expect(mockPostDelete).toHaveBeenCalledWith({
      org: 'org',
      bucket: 'defaultBucket',
      body: {
        predicate: '_measurement="measurement"',
        start: '1970-01-01T00:00:00Z',
        stop: '2022-10-22T10:28:10.000Z',
      },
    });

    spy.mockRestore();
  });
});
