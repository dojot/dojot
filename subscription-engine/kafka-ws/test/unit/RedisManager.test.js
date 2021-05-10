const mockConfig = {
  redis: {
    host: 'redis',
    port: 6379,
    database: 1,
    connect_timeout: 3600000,
    'strategy.reconnect.after': 500,
  },
  log: {
    verbose: false,
    'console.level': 'info',
    'file.enable': false,
    'file.level': 'info',
    'file.filename': 'kafka-ws-logs-%DATE%.log',
  },
};

const mockShutdown = jest.fn().mockResolvedValue('ok');
const mockSignalReady = jest.fn();
const mockMicroServiceSdk = {
  ConfigManager: {
    getConfig: jest.fn(() => mockConfig),
    transformObjectKeys: jest.fn((obj) => obj),
  },
  Logger: jest.fn(() => ({
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
  })),
  ServiceStateManager: jest.fn(() => ({
    registerService: jest.fn(),
    signalReady: mockSignalReady,
    signalNotReady: jest.fn(),
    addHealthChecker: jest.fn((service, callback) => callback(jest.fn(), jest.fn())),
    registerShutdownHandler: jest.fn(),
    shutdown: mockShutdown,
  })),
};

const mockQuit = jest.fn();
const reqEventMapMock = {};

const mockRedis = {
  createClient: jest.fn(() => ({
    on: jest.fn().mockImplementation((event, onCallback) => {
      if (!reqEventMapMock[event]) {
        reqEventMapMock[event] = [];
      }
      reqEventMapMock[event].push(onCallback);
    }),
    quit: mockQuit,
  })),
};

jest.mock('redis', () => mockRedis);

jest.mock('@dojot/microservice-sdk', () => mockMicroServiceSdk);

const RedisManger = require('../../app/Redis/RedisManager');

class ErrorRedis extends Error {
  constructor(message, code) {
    super(message);
    this.code = code;
  }
}
beforeEach(() => {
  jest.clearAllMocks();
});

describe('RedisManger', () => {
  test('init', () => {
    reqEventMapMock.connect[0]();
    reqEventMapMock.ready[0]();

    expect(mockSignalReady)
      .toHaveBeenNthCalledWith(1, 'redis');
  });

  test('Can\'t reconnect, Shutdown ok ', () => {
    reqEventMapMock.error[0](new ErrorRedis('msg', 'CONNECTION_BROKEN'));
    expect(mockShutdown)
      .toHaveBeenCalled();
  });

  test('Can\'t reconnect, Shutdown with error ', () => {
    mockShutdown.mockRejectedValue(new Error());
    reqEventMapMock.error[0](new ErrorRedis('msg', 'CONNECTION_BROKEN'));
    expect(mockShutdown)
      .toHaveBeenCalled();
  });

  it('Shutdown redis successfully', async () => {
    mockQuit
      .mockImplementationOnce((callback) => callback(null));

    await RedisManger.shutdownProcess();

    expect(mockQuit)
      .toHaveBeenCalledTimes(1);
  });

  it('Shutdown redis with error', async () => {
    const msgError = 'x';
    mockQuit
      .mockImplementationOnce((callback) => callback(new Error(msgError), null));
    expect.assertions(1);
    try {
      await RedisManger.shutdownProcess();
    } catch (e) {
      expect(mockQuit)
        .toHaveBeenCalledTimes(1);
    }
  });
});
