jest.mock('mongoose');
jest.mock('@dojot/dojot-module-logger');

const EventEmitter = require('events');
const mongoose = require('mongoose');
const { logger } = require('@dojot/dojot-module-logger');

const logMessages = [];
const logOnArray = (error) => {
  if (typeof error === 'string') {
    logMessages.push(error);
  } else if (error instanceof Error) {
    logMessages.push(error.message);
  }
};
logger.debug.mockImplementation(logOnArray);
logger.error.mockImplementation(logOnArray);
logger.info.mockImplementation(logOnArray);

mongoose.connection = new EventEmitter();
mongoose.connection.close = jest.fn();

const db = require('../../src/db');

describe('testing internal functions of the DB module', () => {
  it('should establish a connection with MongoDB', async () => {
    mongoose.connect.mockResolvedValue();
    await expect(db.connect()).resolves.toBeUndefined();
  });

  it('should fail to establish a connection with MongoDB', async () => {
    mongoose.connect.mockRejectedValue(new Error('connection failure'));
    await expect(db.connect()).resolves.toBeUndefined();
    expect(logMessages).toContain('Mongoose connect() failed with error: connection failure');
  });

  it('log an error', async () => {
    mongoose.connection.emit('error', new Error('connection failure'));
    expect(logMessages).toContain('connection failure');
  });

  it('this should emit events that indicate a healthy connection', async () => {
    mongoose.connection.emit('connected');
    expect(db.healthCheck()).toBeTruthy();
    expect(logMessages).toContain('Connection established to MongoDB');

    mongoose.connection.emit('reconnected');
    expect(db.healthCheck()).toBeTruthy();
    expect(logMessages).toContain('Reconnected to MongoDB');

    mongoose.connection.emit('fullsetup');
    expect(db.healthCheck()).toBeTruthy();
    expect(logMessages).toContain('Connected to the primary and at least one secondary server on the MongoDB Replica Set');

    mongoose.connection.emit('all');
    expect(db.healthCheck()).toBeTruthy();
    expect(logMessages).toContain('Connected to all servers on the MongoDB Replica Set');
  });

  it('should emit events that indicate connection problems', async () => {
    jest.useFakeTimers();

    mongoose.connection.emit('disconnected');
    expect(db.healthCheck()).toBeFalsy();
    expect(logMessages).toContain('Lost MongoDB connection');

    mongoose.connection.emit('reconnectFailed');
    expect(db.healthCheck()).toBeFalsy();
    expect(logMessages).toContain('Reconnected to MongoDB failed');

    mongoose.connection.emit('close');
    expect(db.healthCheck()).toBeFalsy();
    expect(logMessages).toContain('MongoDB connection has been closed');

    jest.runOnlyPendingTimers();
  });
});
