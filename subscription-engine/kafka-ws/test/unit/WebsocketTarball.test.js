/**
 * Unit test for websocketTarballs Module
 */

/**
  * mocks
  */
jest.mock('../../app/Kafka/KafkaTopicsConsumerCallbacksMgmt');
jest.mock('../../app/Redis/RedisExpireMgmt');

const mockMicroServiceSdk = {
  ConfigManager: {
    getConfig: jest.fn(() => jest.fn()),
    transformObjectKeys: jest.fn((obj) => obj),
  },
  Kafka: {
    Consumer: jest.fn(),
  },
  ServiceStateManager: jest.fn(() => ({
    registerService: jest.fn(),
    signalReady: jest.fn(),
    signalNotReady: jest.fn(),
    addHealthChecker: jest.fn((service, callback) => callback(jest.fn(), jest.fn())),
  })),
  Logger: jest.fn(() => ({
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
  })),
};

jest.mock('@dojot/microservice-sdk', () => mockMicroServiceSdk);

const webSocketTarball = require('../../app/WebsocketTarball');

describe('Test websocket tarball', () => {
  beforeEach((done) => {
    jest.resetAllMocks();
    done();
  });

  beforeAll((done) => {
    done();
  });

  it('should init the websocket sucessfully', () => {
    webSocketTarball.init();
  });

  it('should test onClose callback', () => {
    webSocketTarball.onClose();
  });
});
