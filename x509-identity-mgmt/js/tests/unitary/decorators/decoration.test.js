const createDecoration = require('../../../src/decorators/decoration');

// Class whose instance will be decorated
class Dummy {
  constructor() {
    this.counter = 0;
    this.counterAsync = 0;
  }

  standStill() {
    this.counter += 1;
  }

  async standStillAsync() {
    await Promise.resolve(this.counterAsync += 1);
  }
}

// Factory whose object produced will be decorated
function dummyFactory() {
  const dummy = {
    counter: 0,
    counterAsync: 0,
  };

  dummy.standStill = () => {
    dummy.counter += 1;
  };

  dummy.standStillAsync = async () => {
    await Promise.resolve(dummy.counterAsync += 1);
  };
  return dummy;
}

describe("Unit tests of script 'decoration.js'", () => {
  let decoration = null;
  let containerCradle = null;

  beforeAll(() => {
    decoration = createDecoration({ levelDebug: true });
    const logExecutionTimeDecorator = {};
    const inspectMethodDecorator = {};

    const cb = ({ target, method, args }) => method.apply(target, args);
    logExecutionTimeDecorator.decorate = jest.fn(cb);
    inspectMethodDecorator.decorate = jest.fn(cb);

    const logExecutionTimeAsyncDecorator = {};
    const inspectMethodAsyncDecorator = {};

    // Workaround because jest.fn() does not have an asynchronous function signature
    logExecutionTimeAsyncDecorator.decorate = async ({ target, method, args }) => {
      logExecutionTimeAsyncDecorator.decorate.calls += 1;
      return method.apply(target, args);
    };
    logExecutionTimeAsyncDecorator.decorate.calls = 0;

    // Workaround because jest.fn() does not have an asynchronous function signature
    inspectMethodAsyncDecorator.decorate = async ({ target, method, args }) => {
      inspectMethodAsyncDecorator.decorate.calls += 1;
      return method.apply(target, args);
    };
    inspectMethodAsyncDecorator.decorate.calls = 0;

    containerCradle = {
      logExecutionTimeDecorator,
      inspectMethodDecorator,
      logExecutionTimeAsyncDecorator,
      inspectMethodAsyncDecorator,
    };
  });

  afterEach(() => {
    jest.clearAllMocks();
    containerCradle.logExecutionTimeAsyncDecorator.decorate.calls = 0;
    containerCradle.inspectMethodAsyncDecorator.decorate.calls = 0;
  });

  it('should decorate the methods of the class', () => {
    const decoratedDummy = decoration.fromDecoratedClass(Dummy);
    const dummy = decoratedDummy(containerCradle);

    dummy.standStill();

    expect(containerCradle.logExecutionTimeDecorator.decorate).toHaveBeenCalledTimes(1);
    expect(containerCradle.inspectMethodDecorator.decorate).toHaveBeenCalledTimes(1);
    expect(dummy.counter).toBe(1);

    expect(containerCradle.logExecutionTimeAsyncDecorator.decorate.calls).toBe(0);
    expect(containerCradle.inspectMethodAsyncDecorator.decorate.calls).toBe(0);
    expect(dummy.counterAsync).toBe(0);
  });

  it('should decorate the async methods of the class', async () => {
    const decoratedDummy = decoration.fromDecoratedClass(Dummy);
    const dummy = decoratedDummy(containerCradle);

    await dummy.standStillAsync();

    expect(containerCradle.logExecutionTimeAsyncDecorator.decorate.calls).toBe(1);
    expect(containerCradle.inspectMethodAsyncDecorator.decorate.calls).toBe(1);
    expect(dummy.counterAsync).toBe(1);

    expect(containerCradle.logExecutionTimeDecorator.decorate).toHaveBeenCalledTimes(0);
    expect(containerCradle.inspectMethodDecorator.decorate).toHaveBeenCalledTimes(0);
    expect(dummy.counter).toBe(0);
  });

  it('should decorate the methods of the classless object', () => {
    const decoratedDummy = decoration.fromDecoratedFactory(dummyFactory);
    const dummy = decoratedDummy(containerCradle);

    dummy.standStill();

    expect(containerCradle.logExecutionTimeDecorator.decorate).toHaveBeenCalledTimes(1);
    expect(containerCradle.inspectMethodDecorator.decorate).toHaveBeenCalledTimes(1);
    expect(dummy.counter).toBe(1);

    expect(containerCradle.logExecutionTimeAsyncDecorator.decorate.calls).toBe(0);
    expect(containerCradle.inspectMethodAsyncDecorator.decorate.calls).toBe(0);
    expect(dummy.counterAsync).toBe(0);
  });

  it('should decorate the async methods of the classless object', async () => {
    const decoratedDummy = decoration.fromDecoratedFactory(dummyFactory);
    const dummy = decoratedDummy(containerCradle);

    await dummy.standStillAsync();

    expect(containerCradle.logExecutionTimeAsyncDecorator.decorate.calls).toBe(1);
    expect(containerCradle.inspectMethodAsyncDecorator.decorate.calls).toBe(1);
    expect(dummy.counterAsync).toBe(1);

    expect(containerCradle.logExecutionTimeDecorator.decorate).toHaveBeenCalledTimes(0);
    expect(containerCradle.inspectMethodDecorator.decorate).toHaveBeenCalledTimes(0);
    expect(dummy.counter).toBe(0);
  });

  it('should throw an exception because the decorator is not asynchronous', () => {
    expect(() => {
      const logExecutionTimeAsyncDecorator = {};
      const inspectMethodAsyncDecorator = {};

      // 'decorate' method is not asynchronous!!!
      logExecutionTimeAsyncDecorator.decorate = jest.fn();
      inspectMethodAsyncDecorator.decorate = jest.fn();

      const decoratedDummy = decoration.fromDecoratedClass(Dummy);
      decoratedDummy({
        ...containerCradle,
        logExecutionTimeAsyncDecorator, // overrides the original decorator
        inspectMethodAsyncDecorator, // overrides the original decorator
      });
    }).toThrow();
  });

  it('should not apply to debug decorators', async () => {
    const localDecoration = createDecoration({ levelDebug: () => false });

    const decoratedDummyClass = localDecoration.fromDecoratedClass(Dummy);
    const dummyFromClass = decoratedDummyClass(containerCradle);

    dummyFromClass.standStill();

    expect(containerCradle.logExecutionTimeDecorator.decorate).toHaveBeenCalledTimes(0);
    expect(containerCradle.inspectMethodDecorator.decorate).toHaveBeenCalledTimes(0);
    expect(dummyFromClass.counter).toBe(1);

    await dummyFromClass.standStillAsync();

    expect(containerCradle.logExecutionTimeAsyncDecorator.decorate.calls).toBe(0);
    expect(containerCradle.inspectMethodAsyncDecorator.decorate.calls).toBe(0);
    expect(dummyFromClass.counterAsync).toBe(1);

    const decoratedDummyFactory = localDecoration.fromDecoratedFactory(dummyFactory);
    const dummyFromFactory = decoratedDummyFactory(containerCradle);

    dummyFromFactory.standStill();

    expect(containerCradle.logExecutionTimeDecorator.decorate).toHaveBeenCalledTimes(0);
    expect(containerCradle.inspectMethodDecorator.decorate).toHaveBeenCalledTimes(0);
    expect(dummyFromFactory.counter).toBe(1);

    await dummyFromFactory.standStillAsync();

    expect(containerCradle.logExecutionTimeAsyncDecorator.decorate.calls).toBe(0);
    expect(containerCradle.inspectMethodAsyncDecorator.decorate.calls).toBe(0);
    expect(dummyFromFactory.counterAsync).toBe(1);
  });
});
