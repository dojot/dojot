/*
 * A limitation with the factory parameter is that, since calls to jest.mock()
 * are hoisted to the top of the file, it's not possible to first define a
 * variable and then use it in the factory. An exception is made for variables
 * that start with the word 'mock'.
 */
const mockLogMessages = [];

jest.mock('mongoose');
jest.mock('@dojot/microservice-sdk', () => {
  const Logger = jest.fn().mockImplementation(() => {
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
  return { Logger };
});

const EventEmitter = require('events');
const mongoose = require('mongoose');
const { Logger } = require('@dojot/microservice-sdk');

mongoose.connection = new EventEmitter();
mongoose.connection.close = jest.fn();

const db = require('../../src/db/mongo-client')({ config: global.config.mongo, logger: new Logger() });

describe('testing internal functions of the DB module', () => {
  it('should establish a connection with MongoDB', async () => {
    mongoose.connect.mockResolvedValue();
    await expect(db.connect()).resolves.toBeUndefined();
  });

  it('should fail to establish a connection with MongoDB', async () => {
    mongoose.connect.mockRejectedValue(new Error('connection failure'));
    await expect(db.connect()).resolves.toBeUndefined();
    expect(mockLogMessages).toContain('Mongoose connect() failed with error: connection failure');
  });

  it('log an error', async () => {
    mongoose.connection.emit('error', new Error('connection failure'));
    expect(mockLogMessages).toContain('connection failure');
  });

  it('this should emit events that indicate a healthy connection', async () => {
    mongoose.connection.emit('connected');
    expect(db.healthCheck()).toBeTruthy();
    expect(mockLogMessages).toContain('Connection established to MongoDB');

    mongoose.connection.emit('reconnected');
    expect(db.healthCheck()).toBeTruthy();
    expect(mockLogMessages).toContain('Reconnected to MongoDB');

    mongoose.connection.emit('fullsetup');
    expect(db.healthCheck()).toBeTruthy();
    expect(mockLogMessages).toContain('Connected to the primary and at least one secondary server on the MongoDB Replica Set');

    mongoose.connection.emit('all');
    expect(db.healthCheck()).toBeTruthy();
    expect(mockLogMessages).toContain('Connected to all servers on the MongoDB Replica Set');
  });

  it('should emit events that indicate connection problems', async () => {
    jest.useFakeTimers();

    mongoose.connection.emit('disconnected');
    expect(db.healthCheck()).toBeFalsy();
    expect(mockLogMessages).toContain('Lost MongoDB connection');

    mongoose.connection.emit('reconnectFailed');
    expect(db.healthCheck()).toBeFalsy();
    expect(mockLogMessages).toContain('Reconnected to MongoDB failed');

    mongoose.connection.emit('close');
    expect(db.healthCheck()).toBeFalsy();
    expect(mockLogMessages).toContain('MongoDB connection has been closed');

    jest.runOnlyPendingTimers();
  });
});
