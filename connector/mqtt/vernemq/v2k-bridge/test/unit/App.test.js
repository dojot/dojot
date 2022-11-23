const { WebUtils } = jest.requireActual('@dojot/microservice-sdk');

const mockConfig = {
  lightship: { a: 'abc' },
  url: {},
  healthcheck: { 'kafka.interval.ms': 30000 },
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
  Logger: jest.fn(() => ({
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
  })),
  WebUtils,
};
jest.mock('@dojot/microservice-sdk', () => mockSdk);

const mockLogger = {
  error: jest.fn(),
  debug: jest.fn(),
  warn: jest.fn(),
  info: jest.fn(),
};

const mockConsumerMessagesInit = jest.fn();
const mockConsumerMessages = {
  init: mockConsumerMessagesInit,
};
jest.mock('../../app/kafka/ConsumerMessages', () => jest.fn().mockImplementation(() => mockConsumerMessages));

const mockAgentMessengerInit = jest.fn();
const mockHealthChecker = jest.fn();
const mockShutdownHandler = jest.fn();
const mockAgentMessenger = jest.fn().mockImplementation(() => ({
  init: mockAgentMessengerInit,
  healthChecker: mockHealthChecker,
  shutdownHandler: mockShutdownHandler,
}));
jest.mock('../../app/AgentMessenger.js', () => mockAgentMessenger);

const mockMqttShutdownHandler = jest.fn();
const mockMqttClient = jest.fn().mockImplementation(() => ({
  shutdownHandler: mockMqttShutdownHandler,
}));

jest.mock('../../app/MQTTClient.js', () => mockMqttClient);

const App = require('../../app/App');

jest.mock('../../app/axios/TenantService', () => jest.fn().mockImplementation(() => ({
  loadTenants: jest.fn(),
})));

const mockRegisterService = jest.fn();
const mockAddHealthChecker = jest.fn();
const mockRegisterShutdownHandler = jest.fn();
const mockServiceState = {
  addHealthChecker: mockAddHealthChecker,
  registerShutdownHandler: mockRegisterShutdownHandler,
  registerService: mockRegisterService,
};

describe('App', () => {
  let app;

  beforeEach(async () => {
    app = new App(mockConfig, mockLogger, mockServiceState);
  });

  afterAll(() => {
    jest.clearAllMocks();
  });

  describe('constructor', () => {
    it('should successfully create a new instance', () => {
      expect(app.tenantService).toBeDefined();
      expect(app.deviceManagerService).toBeDefined();
      expect(app.consumerMessages).toBeDefined();
      expect(app.agentMessenger).toBeDefined();
      expect(app.mqttClient).toBeDefined();
    });
  });

  describe('init', () => {
    beforeEach(() => {
      jest.clearAllMocks();
    });
    it('should correctly initialize', async () => {
      await app.init();

      mockHealthChecker.mockReturnValue();
      mockShutdownHandler.mockReturnValue();
      mockMqttShutdownHandler.mockReturnValue();
      await expect(mockConsumerMessagesInit).toHaveBeenCalled();
      expect(mockAgentMessengerInit).toHaveBeenCalled();
    });

    it('should not correctly initialize - Promise rejected', async () => {
      const reason = 'error';
      mockConsumerMessagesInit.mockReturnValue(Promise.reject(reason));

      try {
        await app.init();
      } catch (error) {
        expect(error).toEqual(reason);
      }
    });
  });
});
