const mockConfig = {
  redis: {
    host: 'redis',
    port: 6379,
    database: 1,
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
  })),
};

jest.mock('@dojot/microservice-sdk', () => mockMicroServiceSdk);
jest.mock('../../app/StateManager');

const RedisManger = require('../../app/Redis/RedisManager');

beforeEach(() => {
  jest.clearAllMocks();
});

describe('isNumber', () => {
  it('Init Shutdown redis successfully', async () => {
    RedisManger.redisClient.quit = jest.fn((callback) => callback());
    await RedisManger.shutdownHandler();
    expect(RedisManger.redisClient.quit).toHaveBeenCalled();
  });
});
