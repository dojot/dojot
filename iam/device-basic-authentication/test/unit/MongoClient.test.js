/*
 * A limitation with the factory parameter is that, since calls to jest.mock()
 * are hoisted to the top of the file, it's not possible to first define a
 * variable and then use it in the factory. An exception is made for variables
 * that start with the word 'mock'.
 */
const mockLogMessages = [];

jest.mock('mongoose');

jest.mock('uuid');

const mockConfig = {
  healthchecker: { 'kafka.interval.ms': 30000 },
  mongo: { conn: { options: {} } },
};

jest.mock('@dojot/microservice-sdk', () => {
  const LogClass = jest.fn().mockImplementation(() => {
    const logOnArray = (error) => {
      if (typeof error === 'string') {
        mockLogMessages.push(error);
      } else if (error instanceof Error) {
        mockLogMessages.push(error.message);
      }
    };
    const logger = {};
    logger.error = jest.fn().mockImplementation(logOnArray);
    logger.warn = jest.fn().mockImplementation(logOnArray);
    logger.info = jest.fn().mockImplementation(logOnArray);
    logger.debug = jest.fn().mockImplementation(logOnArray);
    return logger;
  });
  return { Logger: LogClass, ConfigManager: { getConfig: jest.fn(() => mockConfig) } };
});

jest.mock('../../app/Utils');

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

const EventEmitter = require('events');
const mongoose = require('mongoose');
const { killApplication } = require('../../app/Utils');

mongoose.connection = new EventEmitter();
mongoose.connection.close = jest.fn();
mongoose.connect = jest.fn();

mongoose.connect.mockRejectedValue('Error');

const MongoClient = require('../../app/db/MongoClient');

describe('mongoClient', () => {
  let mongoClient = null;

  beforeEach(() => {
    mongoClient = new MongoClient(mockServiceState);
    mockLogMessages.length = 0;
  });

  afterAll(() => {
    jest.clearAllMocks();
  });

  describe('constructor', () => {
    it('should successfully create constructor', () => {
      expect(mongoClient.mongo).toBeDefined();
      expect(mongoClient.initialized).toBeFalsy();
      expect(mongoClient.serviceState).toEqual(mockServiceState);
    });
  });

  describe('init', () => {
    beforeAll(() => {
      jest.clearAllMocks();
    });

    beforeEach(async () => {
      await mongoClient.init();
    });

    it('log an error', async () => {
      mongoose.connection.emit('error', new Error('connection failure'));
      expect(mockLogMessages).toContain('connection failure');
    });

    it('this should emit events that indicate a healthy connection', async () => {
      mongoose.connection.emit('connected');
      expect(mockServiceState.signalReady).toHaveBeenCalled();
      expect(mockLogMessages).toContain('Connection established to MongoDB');

      mongoose.connection.emit('reconnected');
      expect(mockServiceState.signalReady).toHaveBeenCalled();
      expect(mockLogMessages).toContain('Reconnected to MongoDB');

      mongoose.connection.emit('fullsetup');
      expect(mockServiceState.signalReady).toHaveBeenCalled();
      expect(mockLogMessages).toContain('Connected to the primary and at least one secondary server on the MongoDB Replica Set');

      mongoose.connection.emit('all');
      expect(mockServiceState.signalReady).toHaveBeenCalled();
      expect(mockLogMessages).toContain('Connected to all servers on the MongoDB Replica Set');
    });

    it('should emit events that indicate connection problems', async () => {
      jest.useFakeTimers();

      mongoose.connection.emit('disconnected');
      expect(mockServiceState.signalNotReady).toHaveBeenCalled();
      expect(mockLogMessages).toContain('Lost MongoDB connection');

      mongoose.connection.emit('reconnectFailed');
      expect(mockServiceState.signalNotReady).toHaveBeenCalled();
      expect(mockLogMessages).toContain('Reconnected to MongoDB failed');

      mongoose.connection.emit('close');
      expect(mockServiceState.signalNotReady).toHaveBeenCalled();
      expect(mockLogMessages).toContain('MongoDB connection has been closed');

      mongoose.connection.emit('disconnected');
      expect(mockServiceState.signalNotReady).toHaveBeenCalled();
      expect(mockLogMessages).toContain('Lost MongoDB connection');

      jest.runOnlyPendingTimers();
      expect(mockLogMessages).toContain('Establishing connection with MongoDB');
    });

    it('should return error', async () => {
      mongoClient.connect = jest.fn();
      mongoClient.connect.mockImplementationOnce(() => {
        throw new Error('Test');
      });
      mongoClient.init();
      expect(killApplication).toHaveBeenCalled();
    });
  });

  describe('connect', () => {
    beforeAll(() => {
      jest.clearAllMocks();
    });

    it('should establish a connection with MongoDB', async () => {
      mongoose.connect.mockResolvedValue();
      await expect(mongoClient.connect()).resolves.toBeUndefined();
    });

    it('should fail to establish a connection with MongoDB', async () => {
      mongoose.connect.mockRejectedValue(new Error('connection failure'));
      await expect(mongoClient.connect()).resolves.toBeUndefined();
      expect(mockLogMessages).toContain('Mongoose connect() failed with error: connection failure');
    });
  });

  describe('shutdownHandler', () => {
    it('should call the disconnect function from the producer', async () => {
      await mongoClient.init();
      mongoClient.mongo = {
        disconnect: jest.fn(),
      };
      const callback = mockRegisterShutdownHandler.mock.calls[0][0];
      callback();
      expect(mockRegisterShutdownHandler).toHaveBeenCalled();
    });
  });
});
