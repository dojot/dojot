/* eslint-disable jest/no-conditional-expect */
/* eslint-disable jest/no-try-expect */
const { WebUtils } = jest.requireActual('@dojot/microservice-sdk');

const mockConfig = {
  lightship: { a: 'abc' },
  url: {},
};

const mockConsumer = {
  init: jest.fn(),
  // eslint-disable-next-line no-unused-vars
  registerCallback: jest.fn((topic, callback) => topic),
  getStatus: jest.fn(() => ({
    connected: true,
  })),
  finish: jest.fn(),
};

const mockSdk = {
  Kafka: jest.fn().mockImplementation(() => ({
    Consumer: mockConsumer,
  })),
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
  WebUtils,
};
jest.mock('@dojot/microservice-sdk', () => mockSdk);

jest.mock('../../app/express');
jest.mock('../../app/express/routes/v1/IncomingMessages');

const mockConsumerMessagesInit = jest.fn();
const mockConsumerMessages = {
  init: mockConsumerMessagesInit,
  getCallbackForNewTenantEvents: jest.fn().mockReturnValue(jest.fn()),
  initCallbackForNewTenantEvents: jest.fn(),
};
jest.mock('../../app/kafka/ConsumerMessages', () => jest.fn().mockImplementation(() => mockConsumerMessages));

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
      await expect(mockConsumerMessagesInit).toHaveBeenCalled();
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
