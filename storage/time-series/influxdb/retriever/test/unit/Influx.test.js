const mockConfig = {
  influx: {
    url: 'abc',
    'default.token': 'abc',
    'default.bucket': 'abc',
  },
};

const mockSdk = {
  ConfigManager: {
    getConfig: jest.fn(() => mockConfig),
  },
  Logger: jest.fn(() => ({
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
  })),
};
jest.mock('@dojot/microservice-sdk', () => mockSdk);

const mockStateIsReady = jest.fn();
const mockStateIsHealth = jest.fn();
const mockInfluxState = jest.fn().mockImplementation(() => ({
  isReady: mockStateIsReady,
  isHealth: mockStateIsHealth,
}));
jest.mock('../../app/influx/State', () => mockInfluxState);

const mockInfluxDataQueryByField = jest.fn();
const mockInfluxDataQuery = jest.fn().mockImplementation(() => ({
  queryByField: mockInfluxDataQueryByField,
  queryByMeasurement: jest.fn(),
}));
jest.mock('../../app/influx/DataQuery', () => mockInfluxDataQuery);

const Influx = require('../../app/influx');


const mockAddHealthChecker = jest.fn();
const serviceStateMock = {
  addHealthChecker: mockAddHealthChecker,
};

describe('Influx', () => {
  let influx = null;
  beforeAll(() => {
    influx = null;
  });

  beforeEach(() => {
    jest.clearAllMocks();
  });

  afterAll(() => {
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  test('instantiate class', () => {
    influx = new Influx(serviceStateMock);
  });

  test('getInfluxDataQueryInstance', () => {
    influx.getInfluxDataQueryInstance().queryByField();
    expect(mockInfluxDataQueryByField).toBeCalled();
  });

  test('getInfluxStateInstance', () => {
    influx.getInfluxStateInstance().isReady();
    expect(mockStateIsReady).toBeCalled();
  });

  test('createInfluxHealthChecker', () => {
    influx.createInfluxHealthChecker();
    expect(mockAddHealthChecker).toBeCalled();
  });
});
