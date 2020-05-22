jest.mock('mongoose');
jest.mock('@dojot/dojot-module-logger');

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

mongoose.connection = {
  on: jest.fn(),
  close: jest.fn(),
};

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
});
