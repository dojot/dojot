const mockConfig = {
  lightship: { a: 'abc' },
  graphql: { graphiql: true },
  sync: {
    tenants: 'apigettenants',
    devices: 'apigetdevices',
  },
  influx: {
    url: 'url',
    'default.token': 'default.token',
    'max.timeout.ms': 1000,
    'default.bucket': 'default_bucket',
  },
};

const mockInfluxDB = {
  InfluxDB: jest.fn().mockImplementation(() => {
  }),
};

jest.mock('@influxdata/influxdb-client', () => mockInfluxDB);

const mockSdk = {
  ConfigManager: {
    getConfig: jest.fn(() => mockConfig),
    transformObjectKeys: jest.fn((obj) => obj),
  },
  ServiceStateManager:
    jest.fn().mockImplementation(() => ({
      registerService: jest.fn(),
    })),
  Logger: jest.fn(() => ({
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
  })),
  WebUtils: {
    framework: {
      errorTemplate: jest.fn(),
    },
  },
  LocalPersistence: {
    LocalPersistenceManager: jest.fn().mockImplementation(() => ({
      init: jest.fn(),
    })),
  },
};
jest.mock('@dojot/microservice-sdk', () => mockSdk);

jest.mock('../../app/express');
jest.mock('../../app/express/routes/v1/Devices');

const mockConsumer = jest.fn().mockImplementation(() => ({
  init: jest.fn(),
  initCallbackForNewTenantEvents: jest.fn(),
  initCallbackForDeviceEvents: jest.fn(),
}));

const mockSyncLoader = jest.fn().mockImplementation(() => ({
  init: jest.fn(),
}));

const mockTenantService = jest.fn().mockImplementation();
const mockDeviceManager = jest.fn().mockImplementation();

jest.mock('../../app/sync/RetrieverConsumer', () => mockConsumer);
jest.mock('../../app/sync/TenantService', () => mockTenantService);
jest.mock('../../app/sync/DeviceManagerService', () => mockDeviceManager);
jest.mock('../../app/sync/SyncLoader', () => mockSyncLoader);

const mockServerRegisterShutdown = jest.fn();
const mockServerInit = jest.fn();
const mockServer = jest.fn().mockImplementation(() => ({
  registerShutdown: mockServerRegisterShutdown,
  init: mockServerInit,
}));
jest.mock('../../app/Server', () => mockServer);

const mockStateIsReady = jest.fn();
const mockCreateInfluxHealthChecker = jest.fn();

jest.mock('../../app/influx/State', () => jest.fn().mockImplementation(() => ({
  isReady: mockStateIsReady,
  createInfluxHealthChecker: mockCreateInfluxHealthChecker,
})));

const App = require('../../app/App');

describe('App', () => {
  let app = null;
  beforeAll(() => {
    app = null;
  });

  beforeEach(() => {
    jest.clearAllMocks();
  });

  afterAll(() => {
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  test('instantiate class and init with error - influx not ready', async () => {
    expect.assertions(1);
    app = new App();
    mockStateIsReady.mockResolvedValueOnce(false);
    try {
      await app.init(() => { });
    } catch (e) {
      expect(e.message).toBe('Influxdb is not ready');
    }
  });

  test('instantiate class and init', async () => {
    app = new App();
    mockStateIsReady.mockResolvedValueOnce(true);
    await app.init(() => { });

    expect(mockCreateInfluxHealthChecker).toBeCalled();
    expect(mockServerRegisterShutdown).toBeCalled();
    expect(mockServerInit).toBeCalled();
  });

  test('instantiate class with  error in constructor', async () => {
    mockServer.mockImplementationOnce(() => {
      throw new Error('Error in Server instantiation.');
    });
    try {
      app = new App();
    } catch (e) {
      expect(e.message).toBe('constructor: Error in Server instantiation.');
    }
  });
});
