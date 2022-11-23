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

const mockConfig = {
  lightship: { a: 'abc' },
  subscribe: {
    'topics.regex.device.manager': '^.+device\'.topic',
  },
  consumer: { 'group.id': '' },
  healthcheck: { 'kafka.interval.ms': 30000 },
};

const mockLogger = {
  debug: jest.fn(),
  error: jest.fn(),
  info: jest.fn(),
  warn: jest.fn(),
};

const mockSdk = {
  Kafka: { Consumer: jest.fn(() => mockConsumer) },
  ConfigManager: { getConfig: jest.fn(() => mockConfig) },
  Logger: jest.fn(() => mockLogger),
};
jest.mock('@dojot/microservice-sdk', () => mockSdk);

const mockGenerateDojotDeviceDataMessage = jest.fn();
const mockKillApplication = jest.fn();
const mockUtils = {
  generateDeviceDataMessage: mockGenerateDojotDeviceDataMessage,
  killApplication: mockKillApplication,
};
jest.mock('../../app/Utils', () => mockUtils);

const mockTenantService = {
  create: jest.fn(() => Promise.resolve('')),
  remove: jest.fn(() => Promise.resolve('')),
};

const ConsumerMessages = require('../../app/kafka/ConsumerMessages');

describe('ConsumerMessages', () => {
  let consumerMessages;

  beforeEach(() => {
    consumerMessages = new ConsumerMessages(
      mockTenantService,
      mockServiceState,
      mockLogger,
    );
  });

  afterAll(() => {
    jest.clearAllMocks();
  });

  describe('constructor', () => {
    it('should successfully create constructor', () => {
      expect(consumerMessages.serviceState).toEqual(mockServiceState);
      expect(consumerMessages.consumer).toBeDefined();
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

      await consumerMessages.init();

      expect(mockConsumer.init).toHaveBeenCalled();
      expect(consumerMessages.createHealthChecker).toHaveBeenCalled();
      expect(consumerMessages.registerShutdown).toHaveBeenCalled();
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

  describe('unregisterCallbacks', () => {
    it('should unregister callbacks in kafka consumer', async () => {
      await consumerMessages.init();

      consumerMessages.consumer.unregisterCallback = jest.fn();
      consumerMessages.idCallbackDeviceManager = 1;

      await consumerMessages.unregisterCallbacks();

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

  describe('initCallbackForTenancyEvents', () => {
    it('Should register callback for new tenant events', async () => {
      consumerMessages.getCallbackForTenancyEvents = jest.fn();
      await consumerMessages.init();

      await consumerMessages.initCallbackForTenancyEvents();

      expect(consumerMessages.getCallbackForTenancyEvents)
        .toHaveBeenCalled();
    });

    it('Should return a callback for new tenant events', async () => {
      const callback = await consumerMessages.getCallbackForTenancyEvents();

      expect(callback).toBeDefined();
    });

    it('Should create a new tenant', async () => {
      const callback = await consumerMessages.getCallbackForTenancyEvents();
      const data = {
        value: '{"type":"CREATE", "tenant": "test", "signatureKey": {}}',
      };
      const ack = jest.fn();
      callback(data, ack);

      expect(mockTenantService.create).toHaveBeenCalled();
    });

    it('Should remove a tenant', async () => {
      const callback = await consumerMessages.getCallbackForTenancyEvents();
      const data = {
        value: '{"type":"DELETE"}',
      };
      const ack = jest.fn();

      callback(data, ack);

      expect(mockTenantService.remove).toHaveBeenCalled();
    });

    it('Should handle the error', async () => {
      const callback = await consumerMessages.getCallbackForTenancyEvents();
      expect.assertions(1);

      const data = {
        value: '{"type":"DELETE"}',
      };
      const ack = jest.fn();
      mockTenantService.remove.mockRejectedValueOnce(new Error('Error'));
      mockLogger.error.mockImplementationOnce((message) => {
        expect(message).toBeDefined();
      });

      callback(data, ack);
    });
  });
});
