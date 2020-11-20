const AgentMessenger = require('../../app/AgentMessenger');

const mockConsumer = {
  finish: jest.fn(),
  getStatus: jest.fn(),
  init: jest.fn(),
  registerCallback: jest.fn(),
};

const mockDefaultConfig = {
  consumer: { },
  healthcheck: {
    'kafka.interval.ms': 30000,
  },
  messenger: {
    'consumer.topic.suffix': 'dojot.device-manager.device',
  },
  sdk: { },
  topic: { },
};

const mockLogger = {
  debug: jest.fn(),
  error: jest.fn(),
  info: jest.fn(),
  warn: jest.fn(),
};

const mockMqttClient = {
  publishMessage: jest.fn(),
};

const mockServiceStateManager = {
  registerService: jest.fn(),
  registerShutdownHandler: jest.fn(),
  addHealthChecker: jest.fn(),
  signalNotReady: jest.fn(),
  signalReady: jest.fn(),
};

jest.mock('@dojot/microservice-sdk', () => ({
  ConfigManager: {
    getConfig: jest.fn(() => mockDefaultConfig),
  },
  Kafka: {
    Consumer: jest.fn(() => mockConsumer),
  },
  Logger: jest.fn(() => mockLogger),
}));

jest.mock('../../app/utils', () => ({
  killApplication: jest.fn(),
}));

describe('AgentMessenger', () => {
  describe('constructor', () => {
    it('should successfully create an Agent Messenger', () => {
      const agentMessenger = new AgentMessenger(mockServiceStateManager);

      expect(agentMessenger.serviceStateManager).toEqual(mockServiceStateManager);
      expect(agentMessenger.wasInitialized).toBeFalsy();
      expect(agentMessenger.stateService).toEqual('kafka');

      expect(agentMessenger.serviceStateManager.registerService).toHaveBeenCalled();
      expect(agentMessenger.serviceStateManager.registerShutdownHandler).toHaveBeenCalled();
      expect(agentMessenger.serviceStateManager.addHealthChecker).toHaveBeenCalled();
    });

    it('should not create an Agent Messenger - no MQTTClient instance was passed', () => {
      expect(() => new AgentMessenger()).toThrow();
    });
  });

  describe('init', () => {
    let agentMessenger;

    beforeEach(() => {
      agentMessenger = new AgentMessenger(mockServiceStateManager);
    });

    it('should successfully initialize the Agent Messenger', async () => {
      mockConsumer.init.mockReturnValue(Promise.resolve());

      await agentMessenger.init(mockMqttClient);

      expect(mockConsumer.registerCallback).toHaveBeenCalled();
      expect(mockServiceStateManager.signalReady).toHaveBeenCalled();
    });

    it('should not initialize the Agent Messenger - is already initialized', async () => {
      // Faster way for faking the initialization
      agentMessenger.wasInitialized = true;

      await agentMessenger.init(mockMqttClient);

      expect(mockConsumer.init).not.toHaveBeenCalled();
    });

    it('should fail the initialization of the Agent Messenger', async () => {
      mockConsumer.init.mockReturnValue(Promise.reject(new Error('fakeError')));

      await agentMessenger.init(mockMqttClient);

      expect(mockConsumer.registerCallback).not.toHaveBeenCalled();
      expect(mockServiceStateManager.signalNotReady).toHaveBeenCalled();
    });

    it('should send a message when the registered callback is called', async () => {
      mockConsumer.init.mockReturnValue(Promise.resolve());

      await agentMessenger.init(mockMqttClient);

      expect(mockConsumer.registerCallback).toHaveBeenCalled();

      // Retrieving the callback passed to registerCallback
      const callback = mockConsumer.registerCallback.mock.calls[0][1];
      callback({});
      expect(mockMqttClient.publishMessage).toHaveBeenCalled();
    });
  });

  describe('finish', () => {
    let agentMessenger;

    beforeEach(async (done) => {
      jest.clearAllMocks();
      agentMessenger = new AgentMessenger(mockServiceStateManager);
      await agentMessenger.init();
      done();
    });

    it('should finish the consumer', async () => {
      mockConsumer.finish.mockResolvedValueOnce();

      await agentMessenger.finish();

      expect(mockConsumer.finish).toHaveBeenCalled();
      expect(mockServiceStateManager.signalNotReady).toHaveBeenCalled();

      expect(agentMessenger.consumer).toBeUndefined();
      expect(agentMessenger.wasInitialized).toBeFalsy();
    });

    it('should finish the consumer - ignored the error', async () => {
      mockConsumer.finish.mockRejectedValueOnce();

      await agentMessenger.finish();

      expect(mockConsumer.finish).toHaveBeenCalled();
      expect(mockServiceStateManager.signalNotReady).toHaveBeenCalled();

      expect(agentMessenger.wasInitialized).toBeFalsy();
    });
  });

  describe('healthChecker', () => {
    let agentMessenger;
    let signalReady;
    let signalNotReady;

    beforeEach(async (done) => {
      jest.clearAllMocks();

      agentMessenger = new AgentMessenger(mockServiceStateManager);
      signalReady = jest.fn();
      signalNotReady = jest.fn();

      await agentMessenger.init();
      done();
    });

    it('should signal as ready - is connected to Kafka', async () => {
      mockConsumer.getStatus.mockReturnValue(Promise.resolve({ connected: true }));

      await agentMessenger.healthChecker(signalReady, signalNotReady);

      expect(signalReady).toHaveBeenCalled();
    });

    it('should signal as not ready - is not connected to Kafka', async () => {
      mockConsumer.getStatus.mockReturnValue(Promise.resolve({ connected: false }));

      await agentMessenger.healthChecker(signalReady, signalNotReady);

      expect(signalNotReady).toHaveBeenCalled();
    });

    it('should signal as not ready - Promise was rejected', async () => {
      mockConsumer.getStatus.mockReturnValue(Promise.reject());

      await agentMessenger.healthChecker(signalReady, signalNotReady);

      expect(signalNotReady).toHaveBeenCalled();
    });

    it('should signal as not ready - consumer is undefined', async () => {
      agentMessenger.consumer = undefined;

      await agentMessenger.healthChecker(signalReady, signalNotReady);

      expect(signalNotReady).toHaveBeenCalled();
    });
  });

  describe('shutdownHandler', () => {
    it('should successfully finish', async () => {
      const agentMessenger = new AgentMessenger(mockServiceStateManager);

      await agentMessenger.init(mockMqttClient);
      await agentMessenger.shutdownHandler();

      expect(mockConsumer.finish).toHaveBeenCalled();
    });
  });
});
