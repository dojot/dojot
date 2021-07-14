const mockConfig = {
  lightship: { a: 'abc' },
  graphql: { graphiql: true },
};

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
};
jest.mock('@dojot/microservice-sdk', () => mockSdk);

jest.mock('../../app/express');
jest.mock('../../app/express/routes/v1/Devices');

const mockServerRegisterShutdown = jest.fn();
const mockServerInit = jest.fn();
const mockServer = jest.fn().mockImplementation(() => ({
  registerShutdown: mockServerRegisterShutdown,
  init: mockServerInit,
}));
jest.mock('../../app/Server', () => mockServer);

const mockStateIsReady = jest.fn();
const mockCreateInfluxHealthChecker = jest.fn();
const mockInflux = jest.fn().mockImplementation(() => ({
  createInfluxHealthChecker: mockCreateInfluxHealthChecker,
  getInfluxStateInstance: jest.fn().mockImplementation(() => ({
    isReady: mockStateIsReady,
  })),
  getInfluxDataQueryInstance: jest.fn().mockImplementation(() => ({
    queryByField: jest.fn(),
    queryByMeasurement: jest.fn(),
    queryUsingGraphql: jest.fn(),
  })),
}));
jest.mock('../../app/influx', () => mockInflux);

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
    mockServer.mockImplementationOnce(
      () => {
        throw new Error('Error in Server instantiation.');
      },
    );
    try {
      app = new App();
    } catch (e) {
      expect(e.message).toBe('constructor: Error in Server instantiation.');
    }
  });
});
