/*
 * A limitation with the factory parameter is that, since calls to jest.mock()
 * are hoisted to the top of the file, it's not possible to first define a
 * variable and then use it in the factory. An exception is made for variables
 * that start with the word 'mock'.
 */
const mockLogMessages = [];

jest.mock('mongoose');
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
  return { Logger: LogClass };
});

const EventEmitter = require('events');
const mongoose = require('mongoose');
const { Logger } = require('@dojot/microservice-sdk');

mongoose.connection = new EventEmitter();
mongoose.connection.close = jest.fn();

const createMongoClient = require('../../../src/db/mongoClient');

const { errorTemplate } = global;

describe("Unit tests of script 'mongoClient.js'", () => {
  let mongoClient = null;
  let healthCheck = null;

  beforeAll(() => {
    healthCheck = {
      ready: jest.fn(),
      notReady: jest.fn(),
    };
    mongoClient = createMongoClient({
      config: global.config.mongo.conn,
      logger: new Logger('mongoClient.js'),
      healthCheck,
      errorTemplate,
    });
  });

  afterEach(() => {
    jest.clearAllMocks();
    mockLogMessages.length = 0;
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

  it('log an error', async () => {
    mongoose.connection.emit('error', new Error('connection failure'));
    expect(mockLogMessages).toContain('connection failure');
  });

  it('this should emit events that indicate a healthy connection', async () => {
    mongoose.connection.emit('connected');
    expect(healthCheck.ready).toHaveBeenCalledTimes(1);
    expect(mockLogMessages).toContain('Connection established to MongoDB');

    mongoose.connection.emit('reconnected');
    expect(healthCheck.ready).toHaveBeenCalledTimes(2);
    expect(mockLogMessages).toContain('Reconnected to MongoDB');

    mongoose.connection.emit('fullsetup');
    expect(healthCheck.ready).toHaveBeenCalledTimes(3);
    expect(mockLogMessages).toContain('Connected to the primary and at least one secondary server on the MongoDB Replica Set');

    mongoose.connection.emit('all');
    expect(healthCheck.ready).toHaveBeenCalledTimes(4);
    expect(mockLogMessages).toContain('Connected to all servers on the MongoDB Replica Set');
  });

  it('should emit events that indicate connection problems', async () => {
    jest.useFakeTimers();

    mongoose.connection.emit('disconnected');
    expect(healthCheck.notReady).toHaveBeenCalledTimes(1);
    expect(mockLogMessages).toContain('Lost MongoDB connection');

    mongoose.connection.emit('reconnectFailed');
    expect(healthCheck.notReady).toHaveBeenCalledTimes(2);
    expect(mockLogMessages).toContain('Reconnected to MongoDB failed');

    mongoose.connection.emit('close');
    expect(healthCheck.notReady).toHaveBeenCalledTimes(3);
    expect(mockLogMessages).toContain('MongoDB connection has been closed');

    mongoose.connection.emit('disconnected');
    expect(healthCheck.notReady).toHaveBeenCalledTimes(4);
    expect(mockLogMessages).toContain('Lost MongoDB connection');

    jest.runOnlyPendingTimers();
    expect(mockLogMessages).toContain('Establishing connection with MongoDB');
  });

  it('should parse projection fields', () => {
    const commaSeparatedFields = 'validity,belongsTo';
    const projectableFields = [
      'validity',
      'validity.notBefore',
      'validity.notAfter',
      'belongsTo',
      'belongsTo.device',
      'belongsTo.application',
    ];
    const fields = mongoClient.parseProjectionFields(commaSeparatedFields, projectableFields);
    expect(fields).toEqual(expect.arrayContaining(['validity', 'belongsTo']));
  });

  it('should parse all projection fields', () => {
    const commaSeparatedFields = '';
    const projectableFields = [
      'validity.notBefore',
      'validity.notAfter',
      'belongsTo.device',
      'belongsTo.application',
    ];
    const fields = mongoClient.parseProjectionFields(commaSeparatedFields, projectableFields);
    expect(fields).toEqual(expect.arrayContaining(projectableFields));
  });

  it('should throw an exception because invalid projection fields were passed', () => {
    const commaSeparatedFields = 'invalid.field';
    const projectableFields = ['a.valid.field'];
    expect(() => {
      mongoClient.parseProjectionFields(commaSeparatedFields, projectableFields);
    }).toThrow();
  });

  it('this should sanitize (in depth) object attributes', () => {
    const obj = {
      toBeRemoved: null,
      validity: {
        toBeRemoved: null,
        notBefore: null,
        notAfter: null,
      },
      belongsTo: {
        toBeRemoved: null,
        device: null,
        application: null,
      },
    };
    const dottedAllowedFields = [
      'validity.notBefore',
      'validity.notAfter',
      'validity.ignored.value',
      'belongsTo.device',
      'belongsTo.application',
    ];
    const result = mongoClient.sanitizeFields(obj, dottedAllowedFields);
    expect(result).toEqual({
      validity: {
        notBefore: null,
        notAfter: null,
      },
      belongsTo: {
        device: null,
        application: null,
      },
    });
  });
});
