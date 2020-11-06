const MQTTClient = require('../../app/MqttClient');
const utils = require('../../app/utils');

// Mock objects
const mockAgentMessenger = {
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
};

const mockMqtt = {
  on: jest.fn(),
  publish: jest.fn(),
};

// Lib mocks
jest.mock('@dojot/microservice-sdk', () => ({
  ConfigManager: {
    getConfig: jest.fn(() => mockDefaultConfig),
    // For simplicity, we will return the same thing
    transformObjectKeys: jest.fn((obj) => obj),
  },
  Logger: jest.fn(() => mockLogger),
}));

jest.mock('fs', () => ({
  readFileSync: jest.fn(),
}));
jest.mock('mqtt', () => ({
  connect: jest.fn(() => mockMqtt),
}));

jest.mock('../../app/AgentMessenger', () => jest.fn(() => mockAgentMessenger));

jest.mock('../../app/utils', () => ({
  generateDojotActuationTopic: jest.fn(() => 'tenant:deviceid/config'),
  killApplication: jest.fn(),
}));

describe.only('MQTTClient', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('constructor', () => {
    it('should successfully create a MQTTClient instance', () => {
      const client = new MQTTClient();

      expect(client.isConnected).toBeFalsy();
      expect(client.publishConfig).toBe(mockDefaultConfig.publish);
      expect(client.mqttOptions).toBeDefined();
      expect(client.mqttClient).toBeNull();
      expect(client.agentMessenger).toBeNull();
      expect(client.logger).toBeDefined();
    });
  });

  describe('init', () => {
    it('should successfully initialize the MQTT client', () => {
      const client = new MQTTClient();

      client.init();

      expect(client.agentMessenger).toBeDefined();
      expect(client.mqttClient).toBeDefined();
      expect(mockMqtt.on).toHaveBeenCalledTimes(3);
    });
  });

  describe('callbacks', () => {
    let client;

    beforeEach(() => {
      client = new MQTTClient();
      client.init();
    });

    describe('onConnect', () => {
      it('should successfully connect to the broker', () => {
        client.onConnect();

        expect(client.isConnected).toBeTruthy();
        expect(mockAgentMessenger.init).toHaveBeenCalled();
      });
    });

    describe('onClose', () => {
      it('should successfully disconnect the client', () => {
        client.onClose();

        expect(client.isConnected).toBeFalsy();
      });
    });

    describe('onError', () => {
      it('should exit the process when an error occurs - no error instance', () => {
        client.onError();

        expect(utils.killApplication).toHaveBeenCalled();
      });

      it('should exit the process when an error occurs - with error instance', () => {
        client.onError(new Error('testError'));

        expect(utils.killApplication).toHaveBeenCalled();
        expect(mockLogger.error).toHaveBeenCalledTimes(3);
      });
    });
  });

  describe('publishMessage', () => {
    let client;
    const topic = 'tenant:deviceid/config';

    beforeEach(() => {
      client = new MQTTClient();
      client.init();
    });

    it('should successfully publish a message', () => {
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

      client.onConnect();
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

      beforeEach(() => {
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

        client.onConnect();
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
});
