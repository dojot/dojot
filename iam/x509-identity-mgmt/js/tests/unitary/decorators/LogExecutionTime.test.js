jest.mock('mongoose');
jest.mock('@dojot/microservice-sdk', () => {
  const LogClass = jest.fn().mockImplementation(() => {
    const logger = {};
    logger.debug = jest.fn();
    return logger;
  });
  return { Logger: LogClass };
});

const { Logger } = require('@dojot/microservice-sdk');

const LogExecutionTime = require('../../../src/decorators/LogExecutionTime');

// Class whose instance will be decorated
class Dummy {
  constructor() {
    this.counter = 0;
  }

  standStill() {
    this.counter += 1;
  }

  getCounter() {
    return this.counter;
  }
}

describe("Unit tests of script 'LogExecutionTime.js'", () => {
  let logExecutionTime = null;
  let logger = null;

  beforeAll(() => {
    logger = new Logger('LogExecutionTime.test.js');
    logExecutionTime = new LogExecutionTime({ logger });
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  it('should decorate a method that returns nothing', () => {
    const target = new Dummy();
    const method = target.standStill;
    const args = [];
    const methodName = 'standStill';

    expect(logExecutionTime.decorate({
      target, method, args, methodName,
    })).toBeUndefined();

    expect(target.counter).toBe(1);
    expect(logger.debug).toHaveBeenCalledTimes(1);
  });

  it('this should decorate a method that returns something', () => {
    const target = new Dummy();
    const method = target.getCounter;
    const args = [];
    const methodName = 'getCounter';

    target.counter = 1010;

    expect(logExecutionTime.decorate({
      target, method, args, methodName,
    })).toBe(target.counter);

    expect(logger.debug).toHaveBeenCalledTimes(1);
  });
});
