const AgentMessenger = require('../../app/AgentMessenger');
const MQTTClient = require('../../app/MqttClient');

const mockConsumer = {
  init: jest.fn(),
  registerCallback: jest.fn(),
};

const mockDefaultConfig = {
  kafka: { },
  messenger: {
    'consume.topic.suffix': 'dojot.device-manager.device',
  },
  sdk: { },
  topic: { },
};

const mockLogger = {
  debug: jest.fn(),
  error: jest.fn(),
  info: jest.fn(),
};

jest.mock('../../app/MqttClient', () => jest.fn(() => ({
  publishMessage: jest.fn(),
})));

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
  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('constructor', () => {
    it('should successfully create an Agent Messenger', () => {
      const agentMessenger = new AgentMessenger(new MQTTClient());

      expect(agentMessenger.mqttClient).toBeDefined();
      expect(agentMessenger.consumer).toBeDefined();
    });

    it('should not create an Agent Messenger - no MQTTClient instance was passed', () => {
      expect(() => new AgentMessenger()).toThrow();
    });
  });

  describe('init', () => {
    let agentMessenger;

    beforeEach(() => {
      agentMessenger = new AgentMessenger(new MQTTClient());
    });

    it('should successfully initialize the Agent Messenger', async () => {
      mockConsumer.init.mockReturnValue(Promise.resolve());

      await agentMessenger.init();

      expect(mockConsumer.registerCallback).toHaveBeenCalledTimes(1);
    });

    it('should not initialize the Agent Messenger - is already initialized', async () => {
      // Faster way for faking the initialization
      agentMessenger.wasInitialized = true;

      await agentMessenger.init();

      expect(mockConsumer.init).not.toHaveBeenCalled();
    });

    it('should fail the initialization of the Agent Messenger', async () => {
      mockConsumer.init.mockReturnValue(Promise.reject(new Error('fakeError')));

      await agentMessenger.init();

      expect(agentMessenger.mqttClient.publishMessage).not.toHaveBeenCalled();
    });

    it('should send a message when the registered callback is called', async () => {
      mockConsumer.init.mockReturnValue(Promise.resolve());

      await agentMessenger.init();

      expect(mockConsumer.registerCallback).toHaveBeenCalledTimes(1);

      // Retrieving the callback passed to registerCallback
      const callback = mockConsumer.registerCallback.mock.calls[0][1];
      callback({});
      expect(agentMessenger.mqttClient.publishMessage).toHaveBeenCalledTimes(1);
    });
  });
});
