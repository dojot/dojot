const mockProcess = require('jest-mock-process');

/**
 * Mocks
 */
const mockAgentMessenger = {
  sendMessage: jest.fn(),
};

const mockAsyncQueue = {
  drain: jest.fn((callback) => callback()),
  push: jest.fn((data, callback) => callback()),
};

const mockConfig = {
  backpressure: {
    handlers: 1,
    'queue.length.max': 10,
  },
  // The keys are already in camelCase to facilitate the code
  mqtt: {
    ca: 'ca.crt',
    cert: 'dev.crt',
    clientId: 'testClient',
    key: 'dev.key',
    protocol: 'mqtts',
  },
  subscription: {
    qos: 1,
    topic: 'testTopic',
  },

  Logger: {
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
  },
};

const mockFs = {
  readFileSync: jest.fn(),
};

const mockMqtt = {
  end: jest.fn(),
  on: jest.fn(),
  reconnect: jest.fn(),
  subscribe: jest.fn(),
  unsubscribe: jest.fn(),
};

const mockMqttClient = {
  connect: jest.fn(() => mockMqtt),
};

const mockSdk = {
  ConfigManager: {
    getConfig: jest.fn(() => mockConfig),
    transformObjectKeys: jest.fn((obj) => obj),
  },
  Kafka: jest.fn(() => ({
    Producer: jest.fn(),
  })),
  ServiceStateManager: {
    registerService: jest.fn(),
    signalReady: jest.fn(),
    signalNotReady: jest.fn(),
  },
  Logger: jest.fn(() => mockConfig.Logger),
};

const mockgetDevice = jest.fn();
const mockDeviceManagerService = {
  getDevice: mockgetDevice,
};
jest.mock(
  '../../app/axios/DeviceManagerService.js',
  () => mockDeviceManagerService,
);

/**
 * Manual mocks
 */
jest.mock('../../app/AgentMessenger', () => mockAgentMessenger);
jest.mock('async', () => ({
  queue: (callback) => {
    callback(jest.fn(), jest.fn());
    return mockAsyncQueue;
  },
}));
jest.mock('fs', () => mockFs);
jest.mock('mqtt', () => mockMqttClient);
jest.mock('@dojot/microservice-sdk', () => mockSdk);

const MQTTClient = require('../../app/MQTTClient');

describe('MQTTClient', () => {
  let mockExit;

  beforeEach(() => {
    mockExit = mockProcess.mockProcessExit();
    jest.clearAllMocks();
  });

  afterAll(() => {
    mockExit.mockRestore();
  });

  describe('constructor', () => {
    it('should successfully create a client', () => {
      mockFs.readFileSync
        .mockReturnValueOnce('caCertValue')
        .mockReturnValueOnce('keyValue')
        .mockReturnValueOnce('certValue');

      const certificates = {
        ca: 'caCertValue',
        cert: 'certValue',
        key: 'keyValue',
      };

      const client = new MQTTClient(
        mockAgentMessenger,
        mockSdk.ServiceStateManager,
        mockConfig.Logger,
        mockDeviceManagerService,
      );

      expect(client.logger).toBeDefined();
      expect(client.agentMessenger).toBeDefined();
      expect(client.serviceStateManager).toBeDefined();
      expect(client.deviceManagerService).toBeDefined();
      expect(client.config).toEqual(mockConfig);
      expect(client.isConnected).toBeFalsy();
      expect(client.mqttOptions).toEqual({ ...mockConfig.mqtt, ...certificates });
      expect(client.messageQueue).toEqual(null);
      expect(client.currentMessageQueueLength).toEqual(0);
    });

    it('should not create a client - no AgentMessenger nor ServiceStateManager instance was passed', () => {
      expect(() => new MQTTClient()).toThrow();
    });
  });

  describe('init', () => {
    let mqttClient;

    beforeEach(() => {
      mqttClient = new MQTTClient(
        mockAgentMessenger,
        mockSdk.ServiceStateManager,
        mockConfig.Logger,
        mockDeviceManagerService,
      );
    });

    it('should successfully initialize the client - MQTTClient not connected', () => {
      const connectSpy = jest.spyOn(mqttClient, 'connect');
      const asyncQueueWorkerSpy = jest.spyOn(mqttClient, 'asyncQueueWorker');

      mqttClient.init();

      expect(connectSpy).toHaveBeenCalled();
      expect(mockMqtt.on).toHaveBeenCalledTimes(5);
      expect(asyncQueueWorkerSpy).toHaveBeenCalled();
      expect(mqttClient.mqttClient).toBeDefined();
      expect(mqttClient.messageQueue).toBeDefined();
      expect(mockAsyncQueue.drain).toHaveBeenCalled();
      expect(mqttClient.isConnected).toBeFalsy();
    });

    it('should successfully initialize the client - MQTTClient already connected', () => {
      const connectSpy = jest.spyOn(mqttClient, 'connect');
      const asyncQueueWorkerSpy = jest.spyOn(mqttClient, 'asyncQueueWorker');

      mqttClient.init();
      // We need to clear the mocks before proceeding
      jest.clearAllMocks();
      mqttClient.isConnected = true;
      mqttClient.secureMode = true;
      mqttClient.init();

      expect(connectSpy).toHaveBeenCalled();
      expect(mockMqtt.on).toHaveBeenCalledTimes(5);
      expect(asyncQueueWorkerSpy).toHaveBeenCalled();
      expect(mqttClient.mqttClient).toBeDefined();
      expect(mqttClient.messageQueue).toBeDefined();
      expect(mockAsyncQueue.drain).toHaveBeenCalled();
      expect(mqttClient.isConnected).toBeTruthy();
    });

    it('should test connect or disconnect the MQTT Client', () => {
      mqttClient.init();

      // We need to clear the mocks before proceeding
      jest.clearAllMocks();
      mqttClient.isConnected = true;

      mqttClient.connectOrDisconnectClient(false);
      expect(mqttClient.mqttClient.end).toHaveBeenCalledWith(true);

      mqttClient.connectOrDisconnectClient(true);

      mqttClient.isConnected = false;
      mqttClient.connectOrDisconnectClient(true);
      expect(mqttClient.mqttClient.reconnect).toHaveBeenCalled();
    });
  });

  describe('Internal functions', () => {
    let mqttClient;

    beforeEach(() => {
      mqttClient = new MQTTClient(
        mockAgentMessenger,
        mockSdk.ServiceStateManager,
        mockConfig.Logger,
        mockDeviceManagerService,
      );
      mqttClient.init();
      jest.clearAllMocks();
    });

    describe('onConnect', () => {
      it('should successfully connect to the broker', () => {
        const subscribeSpy = jest.spyOn(mqttClient, 'subscribe');
        expect(mqttClient.isConnected).toBeFalsy();

        mqttClient.onConnect();

        expect(mqttClient.isConnected).toBeTruthy();
        expect(subscribeSpy).toHaveBeenCalled();
      });
    });

    describe('onReconnect', () => {
      it('should successfully reconnect to the broker', () => {
        mqttClient.onReconnect();

        expect(mqttClient.isConnected).toBeFalsy();
      });
    });

    describe('onClose', () => {
      it('should reconnect the client after a disconnection', () => {
        mqttClient.onClose();

        expect(mqttClient.isConnected).toBeFalsy();
        expect(mockSdk.ServiceStateManager.signalNotReady).toHaveBeenCalled();
      });
    });

    describe('onError', () => {
      it('should exit after an error has occurred', () => {
        mqttClient.onError(new Error('testError'));

        expect(mqttClient.isConnected).toBeFalsy();
        expect(mockSdk.ServiceStateManager.signalNotReady).toHaveBeenCalled();
      });
    });

    describe('onMessage', () => {
      it('should push a message to the queue (callback) once - dup is false', () => {
        mqttClient.isConnected = true;
        mqttClient.onMessage('any', 'any', { dup: false });
        expect(mockAsyncQueue.push).toHaveBeenCalledTimes(1);
      });

      it('should push a message to the queue (callback) once - dup is true', () => {
        mqttClient.isConnected = true;
        mqttClient.onMessage('any', 'any', { dup: true });
        expect(mockAsyncQueue.push).toHaveBeenCalledTimes(0);
      });

      it('should not push a message to the queue (callback) - queue is full', () => {
        mqttClient.isConnected = false;
        mqttClient.onMessage();
        mqttClient.currentMessageQueueLength = 10 * 1000 * 10000;
        mqttClient.onMessage();

        expect(mockAsyncQueue.push).not.toHaveBeenCalled();
        expect(mockMqtt.end).toHaveBeenCalled();
      });
    });

    describe('connect', () => {
      it('should successfully connect to the broker', () => {
        mqttClient.connect();
        expect(mockMqttClient.connect).toHaveBeenCalledTimes(1);
      });

      it('should not connect to the broker - client is already connected', () => {
        mqttClient.isConnected = true;
        mqttClient.connect();
        expect(mockMqttClient.connect).not.toHaveBeenCalled();
      });
    });

    describe('subscribe', () => {
      it('should successfully subscribe - granted array has one object', () => {
        mqttClient.subscribe();

        expect(mockMqtt.unsubscribe).toHaveBeenCalled();
        expect(mockMqtt.subscribe).toHaveBeenCalled();

        const cb = mockMqtt.subscribe.mock.calls[0][2];
        cb(undefined, [{}]);

        expect(mockSdk.ServiceStateManager.signalReady).toHaveBeenCalled();
      });

      it('should not have been subscribed - granted array is empty', () => {
        mqttClient.subscribe();

        expect(mockMqtt.unsubscribe).toHaveBeenCalled();
        expect(mockMqtt.subscribe).toHaveBeenCalled();

        const cb = mockMqtt.subscribe.mock.calls[0][2];
        cb(undefined, []);

        expect(mockExit).toHaveBeenCalled();
      });
    });

    describe('asyncQueueWorker', () => {
      it('should send a message', async () => {
        const fakeMessage = { topic: 'topic', message: '{ "name":"John", "age":30, "city":"New York"}' };
        const { topic, message } = fakeMessage;
        mqttClient.isDeviceDisabled = jest.fn();
        mqttClient.isDeviceDisabled.mockReturnValue(Promise.resolve(false));
        await mqttClient.asyncQueueWorker(fakeMessage);
        expect(mockAgentMessenger.sendMessage).toHaveBeenCalledWith(topic, message);
      });

      it('shouldnt send a message', async () => {
        const fakeMessage = { topic: 'topic', message: '{ "name":"John", "age":30, "city":"New York"}' };
        mqttClient.isDeviceDisabled = jest.fn();
        mqttClient.isDeviceDisabled.mockReturnValue(Promise.resolve(true));
        await mqttClient.asyncQueueWorker(fakeMessage);
        expect(mockAgentMessenger.sendMessage).toHaveBeenCalledTimes(0);
      });

      it('should throw a error, when the request failed', async () => {
        const fakeMessage = { topic: 'topic', message: '{ "name":"John", "age":30, "city":"New York"}' };
        mqttClient.isDeviceDisabled = jest.fn();
        mqttClient.isDeviceDisabled.mockReturnValue(Promise.reject(new Error('Error')));
        await mqttClient.asyncQueueWorker(fakeMessage);
        expect(mockAgentMessenger.sendMessage).toHaveBeenCalledTimes(0);
      });
    });

    describe('isDeviceDisabled', () => {
      it('should return disabled is true', async () => {
        const fakeMessage = { topic: 'topic', message: '{ "name":"John", "age":30, "city":"New York"}' };
        const { topic, message } = fakeMessage;
        mockDeviceManagerService.getDevice.mockReturnValue({ disabled: true });
        const result = await mqttClient.isDeviceDisabled(topic, message);
        expect(result === true);
      });

      it('should return disabled is false', async () => {
        const fakeMessage = { topic: 'topic', message: '{ "name":"John", "age":30, "city":"New York"}' };
        const { topic, message } = fakeMessage;
        mockDeviceManagerService.getDevice.mockReturnValue({ disabled: false });
        const result = await mqttClient.isDeviceDisabled(topic, message);
        expect(result === false);
      });
    });

    describe('shutdownHandler', () => {
      it('should close the connection', (done) => {
        mqttClient.shutdownHandler();

        expect(mockMqtt.unsubscribe).toHaveBeenCalled();
        expect(mockMqtt.end).toHaveBeenCalled();

        done();
      });
    });
  });
});
