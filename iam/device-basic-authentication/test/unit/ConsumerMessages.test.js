/* eslint-disable jest/no-conditional-expect */
// MOCKS
const mockConsumer = {
  init: jest.fn(),
  // eslint-disable-next-line no-unused-vars
  registerCallback: jest.fn((topic, callback) => topic),
  getStatus: jest.fn(() => ({
    connected: true,
  })),
  finish: jest.fn(),
};

const mockAddHealthChecker = jest.fn();
const mockRegisterShutdownHandler = jest.fn();
const mockSignalReady = jest.fn();
const mockSignalNotReady = jest.fn();
const mockServiceState = {
  addHealthChecker: mockAddHealthChecker,
  registerShutdownHandler: mockRegisterShutdownHandler,
  signalReady: mockSignalReady,
  signalNotReady: mockSignalNotReady,
};

const mockBasicCredentials = {
  removeAllFromTenant: jest.fn(),
  remove: jest.fn(),
};

const mockConfig = {
  lightship: { a: 'abc' },
  messenger: {
    'consume.topic.tenancy': 'tenancy.topic',
    'consume.topic.suffix.device.manager': 'device.topic',
  },
  consumer: { 'group.id': '' },
  healthchecker: { 'kafka.interval.ms': 30000 },
};

const mockSdk = {
  Kafka: { Consumer: jest.fn(() => mockConsumer) },
  ConfigManager: { getConfig: jest.fn(() => mockConfig) },
  Logger: jest.fn(() => ({
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
  })),
};
jest.mock('@dojot/microservice-sdk', () => mockSdk);

const mockGenerateDojotDeviceDataMessage = jest.fn();
const mockKillApplication = jest.fn();
const mockUtils = {
  generateDeviceDataMessage: mockGenerateDojotDeviceDataMessage,
  killApplication: mockKillApplication,
};
jest.mock('../../app/Utils', () => mockUtils);

const ConsumerMessages = require('../../app/kafka/ConsumerMessages');

describe('ConsumerMessages', () => {
  let consumerMessages;

  beforeEach(() => {
    consumerMessages = new ConsumerMessages(mockServiceState, mockBasicCredentials);
  });

  afterAll(() => {
    jest.clearAllMocks();
  });

  describe('constructor', () => {
    it('should successfully create constructor', () => {
      expect(consumerMessages.serviceState).toEqual(mockServiceState);
      expect(consumerMessages.basicCredentials).toEqual(mockBasicCredentials);
      expect(consumerMessages.consumer).toBeDefined();
      expect(consumerMessages.idCallbackTenant).toBeDefined();
      expect(consumerMessages.idCallbackDeviceManager).toBeDefined();
      expect(consumerMessages.isReady).toBeFalsy();
    });
  });

  describe('init', () => {
    beforeEach(() => {
      jest.clearAllMocks();
    });

    it('should correctly initialize', async () => {
      mockConsumer.init.mockReturnValue(Promise.resolve());
      consumerMessages.createHealthChecker = jest.fn();
      consumerMessages.registerShutdown = jest.fn();
      consumerMessages.initCallbackForNewTenantEvents = jest.fn();
      consumerMessages.initCallbackForDeviceEvents = jest.fn();

      await consumerMessages.init();

      expect(mockConsumer.init).toHaveBeenCalled();
      expect(consumerMessages.createHealthChecker).toHaveBeenCalled();
      expect(consumerMessages.registerShutdown).toHaveBeenCalled();
      expect(consumerMessages.initCallbackForNewTenantEvents).toHaveBeenCalled();
      expect(consumerMessages.initCallbackForDeviceEvents).toHaveBeenCalled();
    });

    it('should not correctly initialize - Promise rejected', async () => {
      const reason = 'error';
      mockConsumer.init.mockReturnValue(Promise.reject(reason));

      try {
        await consumerMessages.init();
      } catch (error) {
        expect(error).toEqual(reason);
      }
    });
  });

  describe('isConnected', () => {
    beforeEach(async () => {
      jest.clearAllMocks();
      await consumerMessages.init();
    });

    it('should return true, when connection is active', async () => {
      const active = await consumerMessages.isConnected();
      expect(active).toEqual(true);
    });

    it('should return false, when connection is disabled', async () => {
      consumerMessages.consumer.getStatus = jest.fn(() => ({
        connected: false,
      }));

      const active = await consumerMessages.isConnected();
      expect(active).toEqual(false);
    });

    it('should return false, when an error happens', async () => {
      consumerMessages.consumer.getStatus = jest.fn();
      consumerMessages.consumer.getStatus.mockImplementationOnce(() => {
        throw new Error('Test');
      });

      const active = await consumerMessages.isConnected();
      expect(active).toEqual(false);
    });
  });

  describe('initCallbackForNewTenantEvents', () => {
    beforeEach(async () => {
      jest.clearAllMocks();
      await consumerMessages.init();
    });

    it('should init callback', async () => {
      consumerMessages.getCallbackForTenantEvents = jest.fn();

      consumerMessages.initCallbackForNewTenantEvents();
      expect(consumerMessages.getCallbackForTenantEvents).toHaveBeenCalled();
    });

    it('should call removeAllFromTenant', async () => {
      const payload = '{"type": "DELETE", "tenant": "tenant1"}';
      const payloadBuf = Buffer.from(payload, 'utf8');
      const data = { value: payloadBuf };

      const callback = consumerMessages.getCallbackForTenantEvents();
      callback(data);
      expect(mockBasicCredentials.removeAllFromTenant).toHaveBeenCalled();
    });
  });

  describe('initCallbackForDeviceEvents', () => {
    beforeEach(async () => {
      jest.clearAllMocks();
      await consumerMessages.init();
    });

    it('should init callback', async () => {
      consumerMessages.getCallbacksForDeviceEvents = jest.fn();

      consumerMessages.initCallbackForDeviceEvents();
      expect(consumerMessages.getCallbacksForDeviceEvents).toHaveBeenCalled();
    });

    it('should call removeAllFromTenant', async () => {
      const payload = '{"event": "remove", "data": {"templates": [], "id": "8239ad", "created": "2021-10-29T04:35:53.223599+00:00", "label": "vitalParametersMonitorDevice", "attrs": {}}, "meta": {"service": "tenant1"}}';
      const payloadBuf = Buffer.from(payload, 'utf8');
      const data = { value: payloadBuf };

      const callback = consumerMessages.getCallbacksForDeviceEvents();
      callback(data);
      expect(mockBasicCredentials.remove).toHaveBeenCalled();
    });
  });

  describe('resume', () => {
    it('should initiate callbacks', async () => {
      consumerMessages.initCallbackForNewTenantEvents = jest.fn();
      consumerMessages.initCallbackForDeviceEvents = jest.fn();

      consumerMessages.resume();

      expect(consumerMessages.initCallbackForNewTenantEvents).toHaveBeenCalled();
      expect(consumerMessages.initCallbackForDeviceEvents).toHaveBeenCalled();
    });
  });

  describe('unregisterCallbacks', () => {
    it('should unregister callbacks in kafka consumer', async () => {
      await consumerMessages.init();

      consumerMessages.consumer.unregisterCallback = jest.fn();
      consumerMessages.idCallbackTenant = 1;
      consumerMessages.idCallbackDeviceManager = 1;

      await consumerMessages.unregisterCallbacks();

      expect(consumerMessages.idCallbackTenant).toBeNull();
      expect(consumerMessages.idCallbackDeviceManager).toBeNull();
    });
  });

  describe('healthChecker', () => {
    let signalReady;
    let signalNotReady;

    beforeEach(async () => {
      jest.clearAllMocks();
      signalReady = jest.fn();
      signalNotReady = jest.fn();
      mockConsumer.init.mockReturnValue(Promise.resolve());
      await consumerMessages.init();
    });

    it('should signal as ready - is connected to Kafka', async () => {
      consumerMessages.isReady = false;
      mockConsumer.getStatus.mockReturnValue(
        Promise.resolve({ connected: true }),
      );

      consumerMessages.createHealthChecker();

      const callback = mockAddHealthChecker.mock.calls[0][1];
      await callback(signalReady, signalNotReady);

      expect(mockAddHealthChecker).toHaveBeenCalled();
      expect(signalNotReady).not.toHaveBeenCalled();
      expect(signalReady).toHaveBeenCalled();
      expect(consumerMessages.isReady).toBeTruthy();
    });

    it('should signal as not ready - is not connected to Kafka', async () => {
      consumerMessages.isReady = true;
      mockConsumer.getStatus.mockReturnValue(
        Promise.resolve({ connected: false }),
      );

      consumerMessages.createHealthChecker();

      const callback = mockAddHealthChecker.mock.calls[0][1];
      await callback(signalReady, signalNotReady);

      expect(mockAddHealthChecker).toHaveBeenCalled();
      expect(signalReady).not.toHaveBeenCalled();
      expect(signalNotReady).toHaveBeenCalled();
      expect(consumerMessages.isReady).toBeFalsy();
    });

    it('should signal as not ready - getStatus returns error', async () => {
      mockConsumer.getStatus.mockImplementationOnce(() => {
        throw new Error('Test');
      });

      consumerMessages.createHealthChecker();

      const callback = mockAddHealthChecker.mock.calls[0][1];
      await callback(signalReady, signalNotReady);

      expect(mockAddHealthChecker).toHaveBeenCalled();
      expect(signalReady).not.toHaveBeenCalled();
      expect(signalNotReady).toHaveBeenCalled();
      expect(consumerMessages.isReady).toBeFalsy();
    });

    it('should signal as not ready - a new Producer instance is not correctly created', async () => {
      consumerMessages.consumer = undefined;

      consumerMessages.createHealthChecker();

      const callback = mockAddHealthChecker.mock.calls[0][1];
      await callback(signalReady, signalNotReady);

      expect(mockAddHealthChecker).toHaveBeenCalled();
      expect(signalReady).not.toHaveBeenCalled();
      expect(signalNotReady).toHaveBeenCalled();
      expect(consumerMessages.isReady).toBeFalsy();
    });
  });

  describe('shutdownHandler', () => {
    it('should call the disconnect function from the producer', async () => {
      await consumerMessages.init();
      await consumerMessages.registerShutdown();
      const callback = mockRegisterShutdownHandler.mock.calls[0][0];
      callback();
      expect(mockRegisterShutdownHandler).toHaveBeenCalled();
    });
  });
});
