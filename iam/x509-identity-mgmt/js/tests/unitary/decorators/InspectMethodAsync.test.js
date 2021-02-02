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

const InspectMethodAsync = require('../../../src/decorators/InspectMethodAsync');

// Class whose instance will be decorated
class Dummy {
  constructor() {
    this.counter = 0;
  }

  async standStill() {
    this.counter += 1;
  }

  async getCounter() {
    return this.counter;
  }
}

describe("Unit tests of script 'InspectMethodAsync.js'", () => {
  let inspectMethodAsync = null;
  let logger = null;

  beforeAll(() => {
    logger = new Logger('InspectMethodAsync.test.js');
    inspectMethodAsync = new InspectMethodAsync({ logger });
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  it('should decorate a method that returns nothing', async () => {
    const target = new Dummy();
    const method = target.standStill;
    const args = [];
    const methodName = 'standStill';

    await expect(inspectMethodAsync.decorate({
      target, method, args, methodName,
    })).resolves.toBeUndefined();

    expect(target.counter).toBe(1);
    expect(logger.debug).toHaveBeenCalledTimes(2);
  });

  it('this should decorate a method that returns something', async () => {
    const target = new Dummy();
    const method = target.getCounter;
    const args = [];
    const methodName = 'getCounter';

    target.counter = 1010;

    await expect(inspectMethodAsync.decorate({
      target, method, args, methodName,
    })).resolves.toBe(target.counter);

    expect(logger.debug).toHaveBeenCalledTimes(2);
  });
});
