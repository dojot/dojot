/* eslint-disable jest/no-conditional-expect */
const mockConfig = {
  lightship: { a: 'abc' },
  url: {
    devices: 'apidevices',
    device: 'apidevice',
    tenants: 'apitenants',
  },
  mongo: { conn: {} },
};
const mockSdk = {
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
  WebUtils: {
    createTokenGen: jest.fn(),
  },
};
jest.mock('@dojot/microservice-sdk', () => mockSdk);

jest.mock('../../app/server/express');
jest.mock('../../app/server/express/routes/Authentication');

const mockServerInit = jest.fn();
const mockServerRegisterShutdown = jest.fn();
const mockServer = jest.fn().mockImplementation(() => ({
  init: mockServerInit,
  registerShutdown: mockServerRegisterShutdown,
}));
jest.mock('../../app/server/HTTPServer', () => mockServer);

const mockMongoClientInit = jest.fn();
const mockMongoClient = jest.fn().mockImplementation(() => ({
  init: mockMongoClientInit,
}));
jest.mock('../../app/db/MongoClient', () => mockMongoClient);

const mockProducerMessagesInit = jest.fn();
const mockProducerMessages = jest.fn().mockImplementation(() => ({
  init: mockProducerMessagesInit,
}));
jest.mock('../../app/kafka/ProducerMessages', () => mockProducerMessages);

const mockConsumerMessagesInit = jest.fn();
const mockConsumerMessages = jest.fn().mockImplementation(() => ({
  init: mockConsumerMessagesInit,
}));
jest.mock('../../app/kafka/ConsumerMessages', () => mockConsumerMessages);

const mockSyncLoaderInit = jest.fn();
const mockSyncLoader = jest.fn().mockImplementation(() => ({
  init: mockSyncLoaderInit,
}));
jest.mock('../../app/sync/SyncLoader', () => mockSyncLoader);

const App = require('../../app/App');

describe('App', () => {
  let app;

  beforeEach(async () => {
    app = new App();
  });

  afterAll(() => {
    jest.clearAllMocks();
  });

  describe('constructor', () => {
    it('should successfully create constructor', () => {
      expect(app.tenantCtrl).toBeDefined();
      expect(app.basicCredentialsCtrl).toBeDefined();
      expect(app.mongoClient).toBeDefined();
      expect(app.producerMessages).toBeDefined();
      expect(app.consumerMessages).toBeDefined();
      expect(app.server).toBeDefined();
      expect(app.deviceService).toBeDefined();
      expect(app.tenantService).toBeDefined();
      expect(app.syncLoader).toBeDefined();
    });
  });

  describe('init', () => {
    beforeEach(() => {
      jest.clearAllMocks();
    });
    it('should correctly initialize', async () => {
      await app.init();

      expect(mockMongoClientInit).toHaveBeenCalled();
      expect(mockSyncLoaderInit).toHaveBeenCalled();
      expect(mockProducerMessagesInit).toHaveBeenCalled();
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
