const MQTTClient = require('../../app/MQTTClient');
const utils = require('../../app/Utils');

// Mock objects
const mockAgentMessenger = {
  finish: jest.fn(),
  init: jest.fn(),
};

const mockDefaultConfig = {
  logger: {
    'console.level': 'debug',
    'file.enable': true,
    'file.filename': 'testFile.log',
    'file.level': 'debug',
    verbose: true,
  },
  // The keys are already in camelCase to facilitate the code
  mqtt: {
    ca: 'ca.crt',
    certificate: 'client.crt',
    clientId: 'tenant:deviceid',
    key: 'client.key',
    port: 8883,
    protocol: 'mqtts',
  },
  publish: {
    qos: 1,
    'topic.suffix': '/config',
  },
};

const mockLogger = {
  info: jest.fn(),
  error: jest.fn(),
  warn: jest.fn(),
};

const mockMqtt = {
  end: jest.fn(),
  on: jest.fn(),
  publish: jest.fn(),
};

const mockServiceStateManager = {
  registerService: jest.fn(),
  registerShutdownHandler: jest.fn(),
  addHealthChecker: jest.fn(),
  signalNotReady: jest.fn(),
  signalReady: jest.fn(),
};

// Lib mocks
jest.mock('@dojot/microservice-sdk', () => ({
  ConfigManager: {
    getConfig: jest.fn(() => mockDefaultConfig),
    // For simplicity, we will return the same thing
    transformObjectKeys: jest.fn((obj) => obj),
  },
  Logger: jest.fn(() => mockLogger),
  ServiceStateManager: jest.fn(() => mockServiceStateManager),
}));

jest.mock('fs', () => ({
  readFileSync: jest.fn(),
}));
jest.mock('mqtt', () => ({
  connect: jest.fn(() => mockMqtt),
}));

jest.mock('../../app/AgentMessenger', () => jest.fn(() => mockAgentMessenger));

jest.mock('../../app/Utils', () => ({
  generateDojotActuationTopic: jest.fn(() => 'tenant:deviceid/config'),
  killApplication: jest.fn(),
}));

describe('MQTTClient', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('constructor', () => {
    it('should successfully create a MQTTClient instance', () => {
      const client = new MQTTClient(mockAgentMessenger, mockServiceStateManager);

      expect(client.logger).toBeDefined();
      expect(client.mqttOptions).toBeDefined();
      expect(client.agentMessenger).toEqual(mockAgentMessenger);
      expect(client.serviceStateManager).toEqual(mockServiceStateManager);
      expect(client.mqttClient).toBeUndefined();

      expect(client.isConnected).toBeFalsy();
      expect(client.publishConfig).toBe(mockDefaultConfig.publish);
    });

    it('should not instantiate the class - obligatory parameters not passed', () => {
      expect(() => new MQTTClient()).toThrow();
      expect(() => new MQTTClient(mockAgentMessenger)).toThrow();
    });
  });

  describe('init', () => {
    it('should successfully initialize the MQTT client', () => {
      const client = new MQTTClient(mockAgentMessenger, mockServiceStateManager);

      client.init();

      expect(client.agentMessenger).toBeDefined();
      expect(client.mqttClient).toBeDefined();
      expect(mockMqtt.on).toHaveBeenCalledTimes(3);
    });
  });

  describe('callbacks', () => {
    let client;

    beforeEach(() => {
      client = new MQTTClient(mockAgentMessenger, mockServiceStateManager);
      client.init();
    });

    describe('onConnect', () => {
      it('should successfully connect to the broker', async () => {
        await client.onConnect();

        expect(client.isConnected).toBeTruthy();

        expect(mockAgentMessenger.init).toHaveBeenCalled();
        expect(mockServiceStateManager.signalReady).toHaveBeenCalled();
      });

      it('should not connect to the broker - already connected', async () => {
        client.isConnected = true;

        await client.onConnect();

        expect(mockAgentMessenger.init).not.toHaveBeenCalled();
        expect(mockServiceStateManager.signalReady).not.toHaveBeenCalled();
      });
    });

    describe('onClose', () => {
      it('should successfully disconnect the client', async () => {
        await client.onClose();

        expect(client.isConnected).toBeFalsy();

        expect(mockAgentMessenger.finish).toHaveBeenCalled();
        expect(mockServiceStateManager.signalNotReady).toHaveBeenCalled();
      });
    });
  });

  describe('publishMessage', () => {
    let client;
    const topic = 'tenant:deviceid/config';

    beforeEach(() => {
      client = new MQTTClient(mockAgentMessenger, mockServiceStateManager);
      client.init();
    });

    it('should successfully publish a message', async () => {
      const attrs = { key: 'value' };
      const value = Buffer.from(
        JSON.stringify(
          {
            meta: {
              service: 'fake',
            },
            data: {
              id: 'fakeId',
              attrs,
            },
          },
        ),
      );
      const data = { value };

      await client.onConnect();
      client.publishMessage(data);

      expect(mockMqtt.publish).toHaveBeenCalledWith(
        topic,
        JSON.stringify(attrs),
        { qos: mockDefaultConfig.publish.qos },
        expect.any(Function),
      );

      expect(utils.generateDojotActuationTopic).toHaveBeenCalled();
      expect(mockMqtt.publish).toHaveBeenCalled();
    });

    it('should not publish a message - MQTT client not connected', () => {
      client.publishMessage();
      expect(mockMqtt.publish).not.toHaveBeenCalled();
      expect(mockLogger.error).toHaveBeenCalled();
    });

    it('should not publish a message - invalid message', () => {
      client.onConnect();
      client.publishMessage({});
      expect(mockMqtt.publish).not.toHaveBeenCalled();
      expect(mockLogger.error).toHaveBeenCalled();
    });

    describe('publish callback', () => {
      let publishCallback;

      beforeEach(async () => {
        const attrs = { key: 'value' };
        const value = Buffer.from(
          JSON.stringify(
            {
              meta: {
                service: 'fake',
              },
              data: {
                id: 'fakeId',
                attrs,
              },
            },
          ),
        );
        const data = { value };

        await client.onConnect();
        client.publishMessage(data);

        // eslint-disable-next-line prefer-destructuring
        publishCallback = mockMqtt.publish.mock.calls[0][3];
      });

      it('should not log an error', () => {
        publishCallback(null, jest.fn());

        expect(mockLogger.error).not.toHaveBeenCalled();
      });

      it('should log an error - no packet', () => {
        publishCallback(new Error('mockError'));

        expect(mockLogger.error).toHaveBeenCalledTimes(1);
      });

      it('should log an error - with packet', () => {
        publishCallback(new Error('mockError'), jest.fn());

        expect(mockLogger.error).toHaveBeenCalledTimes(2);
      });
    });
  });

  describe('shutdownHandler', () => {
    it('should close the connection', () => {
      const mqttClient = new MQTTClient(mockAgentMessenger, mockServiceStateManager);
      mqttClient.init();

      mockMqtt.end.mockResolvedValueOnce();

      mqttClient.shutdownHandler();

      expect(mockMqtt.end).toHaveBeenCalled();
    });
  });
});
