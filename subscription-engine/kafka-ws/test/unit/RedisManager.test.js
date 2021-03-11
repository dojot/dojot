const mockConfig = {
  redis: {
    host: 'redis',
    port: 6379,
    database: 1,
    connect_timeout: 3600000,
    'max.attemps': 2,
    'strategy.connect.timeout': 500,
  },
  log: {
    verbose: false,
    'console.level': 'info',
    'file.enable': false,
    'file.level': 'info',
    'file.filename': 'kafka-ws-logs-%DATE%.log',
  },
};

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
    signalReady: jest.fn(),
    signalNotReady: jest.fn(),
    addHealthChecker: jest.fn((service, callback) => callback(jest.fn(), jest.fn())),
    registerShutdownHandler: jest.fn(),
    shutdown: jest.fn(),
  })),
};

const mockRedis = {
  createClient: jest.fn(() => ({
    on: jest.fn(),
    quit: jest.fn((callback) => callback()),
  })),
};

jest.mock('redis', () => mockRedis);

jest.mock('@dojot/microservice-sdk', () => mockMicroServiceSdk);
jest.mock('../../app/StateManager');

const RedisManger = require('../../app/Redis/RedisManager');
const StateManager = require('../../app/StateManager');

beforeEach(() => {
  jest.clearAllMocks();
});

describe('isNumber', () => {
  it('Init Shutdown redis successfully', async () => {
    await RedisManger.shutdownProcess();
    expect(RedisManger.redisClient.quit).toHaveBeenCalled();
  });
});

describe('should test redis strategy', () => {
  it('Init Shutdown redis successfully', async () => {
    const opions = { attempt: 1 };
    RedisManger.redisManagerStrategy(opions);
    expect(StateManager.shutdown).toHaveBeenCalledTimes(0);

    opions.attempt = mockConfig.redis['max.attemps'];
    RedisManger.redisManagerStrategy(opions);
    expect(StateManager.shutdown).toHaveBeenCalledTimes(1);
  });
});
