/* eslint-disable jest/no-conditional-expect */
/* eslint-disable jest/no-try-expect */
const mockConfig = {
  lightship: { a: 'abc' },
  url: {},
};

const mockSdk = {
  WebUtils: {
    DojotClientHttp: jest.fn().mockImplementation(() => ({})),
  },
  ConfigManager: {
    getConfig: jest.fn(() => mockConfig),
    transformObjectKeys: jest.fn((obj) => obj),
  },
  ServiceStateManager: jest.fn().mockImplementation(() => ({
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
jest.mock('../../app/express/routes/v1/IncomingMessages');

const mockServerInit = jest.fn();
const mockServerRegisterShutdown = jest.fn();
const mockServer = jest.fn().mockImplementation(() => ({
  init: mockServerInit,
  registerShutdown: mockServerRegisterShutdown,
}));
jest.mock('../../app/Server', () => mockServer);

const mockRedisInit = jest.fn();
const mockRedis = jest.fn().mockImplementation(() => ({
  init: mockRedisInit,
}));
jest.mock('../../app/redis/RedisManager.js', () => mockRedis);

const mockProducerMessagesInit = jest.fn();
const mockProducerMessages = jest.fn().mockImplementation(() => ({
  init: mockProducerMessagesInit,
}));
jest.mock('../../app/kafka/ProducerMessages', () => mockProducerMessages);

const App = require('../../app/App');

jest.mock('../../app/axios/TenantService', () => jest.fn().mockImplementation(() => ({
  loadTenants: jest.fn(),
})));

describe('App', () => {
  let app;

  beforeEach(async () => {
    app = new App(mockConfig);
  });

  afterAll(() => {
    jest.clearAllMocks();
  });

  describe('constructor', () => {
    it('should successfully create a new instance', () => {
      expect(app.server).toBeDefined();
      expect(app.producerMessages).toBeDefined();
      expect(app.redisManager).toBeDefined();
      expect(app.deviceAuthService).toBeDefined();
      expect(app.certificateAclService).toBeDefined();
    });
  });

  describe('init', () => {
    beforeEach(() => {
      jest.clearAllMocks();
    });
    it('should correctly initialize', async () => {
      await app.init();

      expect(mockRedisInit).toHaveBeenCalled();
      await expect(mockProducerMessagesInit).toHaveBeenCalled();
      expect(mockServerInit).toHaveBeenCalled();
      expect(mockServerRegisterShutdown).toHaveBeenCalled();
    });

    it('should not correctly initialize - Promise rejected', async () => {
      const reason = 'error';
      mockProducerMessagesInit.mockReturnValue(Promise.reject(reason));

      try {
        await app.init();
      } catch (error) {
        expect(error).toEqual(reason);
      }
    });
  });
});
