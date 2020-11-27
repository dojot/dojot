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

const mockInfluxDataWriterWrite = jest.fn();
const mockInfluxDataWriter = jest.fn().mockImplementation(() => ({
  write: mockInfluxDataWriterWrite,
  queryByMeasurement: jest.fn(),
}));
jest.mock('../../app/influx/DataWriter', () => mockInfluxDataWriter);

const mockInfluxDataQueryByField2 = jest.fn();
const mockInfluxOrg = jest.fn().mockImplementation(() => ({
  queryByField: mockInfluxDataQueryByField2,
  queryByMeasurement: jest.fn(),
}));
jest.mock('../../app/influx/Organizations', () => mockInfluxOrg);

const mockInfluxDataQueryByField3 = jest.fn();
const mockInfluxMeasurements = jest.fn().mockImplementation(() => ({
  queryByField: mockInfluxDataQueryByField3,
  queryByMeasurement: jest.fn(),
}));
jest.mock('../../app/influx/Measurements', () => mockInfluxMeasurements);


const mockFlat = {
  unflatten: jest.fn(),
  flatten: jest.fn(),
};


jest.mock('flat', () => mockFlat);
jest.mock('lodash.camelcase');

const Influx = require('../../app/influx');


//   const InfluxOrgs = require('./Organizations');
//   const InfluxMeasurement = require('./Measurements');

//   const logger = new Logger('influxdb-storer:Influx');
//   const { influx: configInflux } = getConfig('STORER');

//   const { write: { options: influxWriteOptions } } = flatten.unflatten(configInflux);
//   const configInfluxWriteOptions = influxWriteOptions ? flatten(influxWriteOptions) : {};
//   const configInfluxWriteOptionsCamelCase = transformObjectKeys(configInfluxWriteOptions, camelCase);


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

  test('getInfluxDataWriterInstance', () => {
    influx.getInfluxDataWriterInstance().write();
    expect(mockInfluxDataQueryByField).toBeCalled();
  });

  //   test('getInfluxStateInstance', () => {
  //     influx.getInfluxStateInstance().isReady();
  //     expect(mockStateIsReady).toBeCalled();
  //   });

//   test('createInfluxHealthChecker', () => {
//     influx.createInfluxHealthChecker();
//     expect(mockAddHealthChecker).toBeCalled();
//   });
});
