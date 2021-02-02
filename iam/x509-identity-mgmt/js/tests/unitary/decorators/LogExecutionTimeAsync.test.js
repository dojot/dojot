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

const LogExecutionTimeAsync = require('../../../src/decorators/LogExecutionTimeAsync');

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

describe("Unit tests of script 'LogExecutionTimeAsync.js'", () => {
  let logExecutionTimeAsync = null;
  let logger = null;

  beforeAll(() => {
    logger = new Logger('LogExecutionTimeAsync.test.js');
    logExecutionTimeAsync = new LogExecutionTimeAsync({ logger });
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  it('should decorate a method that returns nothing', async () => {
    const target = new Dummy();
    const method = target.standStill;
    const args = [];
    const methodName = 'standStill';

    await expect(logExecutionTimeAsync.decorate({
      target, method, args, methodName,
    })).resolves.toBeUndefined();

    expect(target.counter).toBe(1);
    expect(logger.debug).toHaveBeenCalledTimes(1);
  });

  it('this should decorate a method that returns something', async () => {
    const target = new Dummy();
    const method = target.getCounter;
    const args = [];
    const methodName = 'getCounter';

    target.counter = 1010;

    await expect(logExecutionTimeAsync.decorate({
      target, method, args, methodName,
    })).resolves.toBe(target.counter);

    expect(logger.debug).toHaveBeenCalledTimes(1);
  });
});
