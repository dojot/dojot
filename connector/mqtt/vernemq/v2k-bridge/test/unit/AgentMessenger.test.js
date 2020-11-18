jest.mock('../../app/Utils');
jest.mock('../../app/MQTTClient.js');

const mockProcess = require('jest-mock-process');
const utils = require('../../app/Utils');
const AgentMessenger = require('../../app/AgentMessenger');

const mockExit = mockProcess.mockProcessExit();

let mockShouldResolve;
let resolveMock;
let rejectMock;

// MOCKS
const mockConfig = {
  ConfigManager: {
    messenger: {
      'produce.topic.suffix': 'device-data',
    },
    producer: {
      acks: -1,
    },
    subscription: {
      qos: 1,
      topic: 'testTopic',
    },
    sdk: {
      'batch.num.messages': 100,
    },
    topic: {
      acks: -1,
    },
  },

  kafkaConfig: {
    test: 'testmock',
    messenger: {
      kafka: {
        dojot: {
          subjects: {
            verne: 'verne',
          },
        },
      },
    },
    mqtt: {
      subscribeTopic: 'topic',
    },
  },

  Logger: {
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
  },

  Producer: {
    connect: jest.fn(),
    disconnect: jest.fn(),
    getStatus: jest.fn(),
    // eslint-disable-next-line no-unused-vars
    produce: jest.fn((topic, message, key) => new Promise((resolve, reject) => {
      // eslint-disable-next-line no-param-reassign
      resolve = jest.fn(resolve);
      // eslint-disable-next-line no-param-reassign
      reject = jest.fn(reject);
      resolveMock = resolve;
      rejectMock = reject;

      if (mockShouldResolve) {
        resolve();
      } else {
        reject(new Error('testError'));
      }
    })),
  },
};

jest.mock('@dojot/microservice-sdk', () => ({
  Kafka: {
    Producer: jest.fn(() => mockConfig.Producer),
  },
  Logger: jest.fn(() => mockConfig.Logger),
  ConfigManager: {
    getConfig: jest.fn(() => mockConfig.ConfigManager),
  },
}));

describe('AgentMessenger', () => {
  let agentMessenger;

  beforeEach(() => {
    agentMessenger = new AgentMessenger();
    jest.clearAllMocks();
  });

  afterAll(() => {
    mockExit.mockRestore();
  });

  describe('constructor', () => {
    it('should successfully create a new instance', () => {
      expect(agentMessenger.config).toEqual(mockConfig.ConfigManager);
      expect(agentMessenger.producer).toBeDefined();
      expect(agentMessenger.logger).toBeDefined();
    });
  });

  describe('init', () => {
    let mqttClient;

    beforeEach(() => {
      mqttClient = {
        init: jest.fn(),
      };
    });

    it('should correctly initialize', async () => {
      mockConfig.Producer.connect.mockReturnValue(Promise.resolve());

      await agentMessenger.init(mqttClient);

      expect(mockConfig.Producer.connect).toHaveBeenCalled();
      expect(mqttClient.init).toHaveBeenCalled();
      expect(agentMessenger.logger).toBeDefined();
    });

    it('should not correctly initialize - Promise rejected', async () => {
      const reason = 'error';
      mockConfig.Producer.connect.mockReturnValue(Promise.reject(reason));

      try {
        await agentMessenger.init(mqttClient);
      } catch (error) {
        expect(error).toEqual(reason);
      }
    });
  });

  describe('sendMessage', () => {
    const generateFakeData = {
      metadata: {
        tenant: 'fake',
        deviceid: 'fake',
      },
    };
    const fakeMessage = '{ "name":"John", "age":30, "city":"New York"}';

    beforeAll(() => {
      utils.generateDojotDeviceDataMessage = jest.fn().mockReturnValue(generateFakeData);
    });

    it('should send message', () => {
      mockShouldResolve = true;

      agentMessenger.sendMessage('test', fakeMessage);

      expect(agentMessenger.producer.produce).toHaveBeenCalled();
      expect(resolveMock).toHaveBeenCalled();
    });

    it('should not send the message - rejected Promise', () => {
      mockShouldResolve = false;

      agentMessenger.sendMessage('test', fakeMessage);

      expect(agentMessenger.producer.produce).toHaveBeenCalled();
      expect(rejectMock).toHaveBeenCalled();
    });

    it('should not send the message - malformed message', () => {
      try {
        agentMessenger.sendMessage('test', 'error format');
      } catch (error) {
        expect(error).toBeDefined();
      } finally {
        expect(mockConfig.Producer.produce).not.toHaveBeenCalled();
      }
    });
  });

  describe('healthChecker', () => {
    let signalReady;
    let signalNotReady;

    beforeEach(() => {
      signalReady = jest.fn();
      signalNotReady = jest.fn();
    });

    it('should signal as ready - is connected to Kafka', async () => {
      mockConfig.Producer.getStatus.mockReturnValue(Promise.resolve({ connected: true }));

      await agentMessenger.healthChecker(signalReady, signalNotReady);

      expect(signalReady).toHaveBeenCalled();
    });

    it('should signal as not ready - is not connected to Kafka', async () => {
      mockConfig.Producer.getStatus.mockReturnValue(Promise.resolve({ connected: false }));

      await agentMessenger.healthChecker(signalReady, signalNotReady);

      expect(signalNotReady).toHaveBeenCalled();
    });

    it('should signal as not ready - Promise was rejected', async () => {
      mockConfig.Producer.getStatus.mockReturnValue(Promise.reject());

      await agentMessenger.healthChecker(signalReady, signalNotReady);

      expect(signalNotReady).toHaveBeenCalled();
    });
  });

  describe('shutdownHandler', () => {
    it('should call the disconnect function from the producer', () => {
      agentMessenger.shutdownHandler();

      expect(agentMessenger.producer.disconnect).toHaveBeenCalled();
    });
  });
});
