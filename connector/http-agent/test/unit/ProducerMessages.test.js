/* eslint-disable jest/no-conditional-expect */
/* eslint-disable jest/no-try-expect */
let mockShouldResolve;
let resolveMock;
let rejectMock;

// MOCKS
const mockProducer = {
  connect: jest.fn(),
  disconnect: jest.fn(),
  getStatus: jest.fn(),
  // eslint-disable-next-line no-unused-vars
  produce: jest.fn(() =>
  // eslint-disable-next-line implicit-arrow-linebreak
    new Promise((resolve, reject) => {
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
};

const mockAddHealthChecker = jest.fn();
const mockRegisterShutdownHandler = jest.fn();
const mockSignalReady = jest.fn();
const mockSignalNotReady = jest.fn();
const serviceStateMock = {
  addHealthChecker: mockAddHealthChecker,
  registerShutdownHandler: mockRegisterShutdownHandler,
  signalReady: mockSignalReady,
  signalNotReady: mockSignalNotReady,
};

const mockConfig = {
  messenger: { 'produce.topic.suffix': 'device-data' },
  producer: { acks: -1 },
  sdkProducer: { 'batch.num.messages': 100 },
  topic: { acks: -1 },
  healthchecker: { 'kafka.interval.ms': 30000 },
};

const mockSdk = {
  Kafka: { Producer: jest.fn(() => mockProducer) },
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

const ProducerMessages = require('../../app/ProducerMessages');

describe('ProducerMessages', () => {
  let producerMessages;

  beforeEach(async () => {
    producerMessages = new ProducerMessages(serviceStateMock);
  });

  afterAll(() => {
    jest.clearAllMocks();
  });

  describe('constructor', () => {
    it('should successfully create a new instance', () => {
      expect(producerMessages.serviceState).toEqual(serviceStateMock);
      expect(producerMessages.producer).toBeDefined();
    });
  });

  describe('init', () => {
    beforeEach(() => {
      jest.clearAllMocks();
    });

    it('should correctly initialize', async () => {
      mockProducer.connect.mockReturnValue(Promise.resolve());

      await producerMessages.init();

      expect(mockProducer.connect).toHaveBeenCalled();
    });

    it('should not correctly initialize - Promise rejected', async () => {
      const reason = 'error';
      mockProducer.connect.mockReturnValue(Promise.reject(reason));

      try {
        await producerMessages.init();
      } catch (error) {
        expect(error).toEqual(reason);
      }
    });
  });

  describe('send', () => {
    beforeEach(() => {
      jest.clearAllMocks();
    });

    const fakeMessage = {
      metadata: {
        timestamp: 168300055,
      },
      attrs: {
        temperature: 25.79,
      },
    };

    it('should send the message', async () => {
      mockShouldResolve = true;

      await producerMessages.init();

      producerMessages.send(
        fakeMessage, 'test', '123abc',
      );

      expect(producerMessages.producer.produce).toHaveBeenCalled();
      expect(resolveMock).toHaveBeenCalled();
    });

    it('should not send the message - rejected Promise', async () => {
      mockShouldResolve = false;

      await producerMessages.init();
      producerMessages.send(
        fakeMessage, 'test', '123abc',
      );

      expect(producerMessages.producer.produce).toHaveBeenCalled();
      expect(rejectMock).toHaveBeenCalled();
    });
  });

  describe('healthChecker', () => {
    let signalReady;
    let signalNotReady;

    beforeEach(async () => {
      jest.clearAllMocks();
      signalReady = jest.fn();
      signalNotReady = jest.fn();
      mockProducer.connect.mockReturnValue(Promise.resolve());
      await producerMessages.init();
    });

    it('should signal as ready - is connected to Kafka', async () => {
      mockProducer.getStatus.mockReturnValue(Promise.resolve({ connected: true }));

      producerMessages.createHealthChecker();

      const callback = mockAddHealthChecker.mock.calls[0][1];
      await callback(signalReady, signalNotReady);

      expect(mockAddHealthChecker).toHaveBeenCalled();
      expect(signalNotReady).not.toHaveBeenCalled();
      expect(signalReady).toHaveBeenCalled();
    });

    it('should signal as not ready - is not connected to Kafka', async () => {
      mockProducer.getStatus.mockReturnValue(Promise.resolve({ connected: false }));

      producerMessages.createHealthChecker();

      const callback = mockAddHealthChecker.mock.calls[0][1];
      await callback(signalReady, signalNotReady);

      expect(mockAddHealthChecker).toHaveBeenCalled();
      expect(signalReady).not.toHaveBeenCalled();
      expect(signalNotReady).toHaveBeenCalled();
    });

    it('should signal as not ready - getStatus returns error', async () => {
      mockProducer.getStatus.mockImplementationOnce(() => {
        throw new Error('Test');
      });

      producerMessages.createHealthChecker();

      const callback = mockAddHealthChecker.mock.calls[0][1];
      await callback(signalReady, signalNotReady);

      expect(mockAddHealthChecker).toHaveBeenCalled();
      expect(signalReady).not.toHaveBeenCalled();
      expect(signalNotReady).toHaveBeenCalled();
    });

    it('should signal as not ready - a new Producer instance is not correctly created', async () => {
      producerMessages.producer = undefined;

      producerMessages.createHealthChecker();

      const callback = mockAddHealthChecker.mock.calls[0][1];
      await callback(signalReady, signalNotReady);

      expect(mockAddHealthChecker).toHaveBeenCalled();
      expect(signalReady).not.toHaveBeenCalled();
      expect(signalNotReady).toHaveBeenCalled();
    });
  });

  describe('shutdownHandler', () => {
    it('should call the disconnect function from the producer', async () => {
      await producerMessages.init();
      await producerMessages.registerShutdown();
      const callback = mockRegisterShutdownHandler.mock.calls[0][0];
      callback();
      expect(mockRegisterShutdownHandler).toHaveBeenCalled();
    });
  });
});
