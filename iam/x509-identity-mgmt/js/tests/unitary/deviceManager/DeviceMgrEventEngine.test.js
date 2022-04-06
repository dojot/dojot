const { Logger } = require('@dojot/microservice-sdk');

const DeviceMgrEventEngine = require('../../../src/deviceManager/DeviceMgrEventEngine');

function deviceMgrKafkaConsumerMock() {
  const deviceMgrKafkaConsumer = {
    init: jest.fn().mockResolvedValue(undefined),
    finish: jest.fn().mockResolvedValue(undefined),
    registerCallback: jest.fn(),
  };
  return deviceMgrKafkaConsumer;
}

function stateManagerMock() {
  const beacon = {
    die: jest.fn(),
  };
  const stateManager = {
    createBeacon: jest.fn(() => beacon),
    beacon,
  };
  return stateManager;
}

function diContainerMock() {
  const deviceMgrEventRunnable = {
    run: jest.fn().mockResolvedValue(undefined),
  };

  const deviceMgrEventRunnableResolver = {
    resolve: () => deviceMgrEventRunnable,
  };

  const scope = {
    cradle: { deviceMgrEventRunnable: deviceMgrEventRunnableResolver },
    register: jest.fn((entries) => {
      scope.cradle = { ...scope.cradle, ...entries };
    }),
    resolve: jest.fn((entry) => {
      const resolver = Reflect.get(scope.cradle, entry);
      if (resolver) {
        return resolver.resolve();
      }
      return null;
    }),
  };

  const diContainer = {
    createScope: jest.fn(() => scope),
    scope,
    runnable: deviceMgrEventRunnable,
  };
  return diContainer;
}

describe("Unit tests of script 'DeviceMgrEventEngine.js'", () => {
  let deviceMgrEventEngine = null;

  beforeAll(() => {
    deviceMgrEventEngine = new DeviceMgrEventEngine({
      kafkaConsumer: deviceMgrKafkaConsumerMock(),
      stateManager: stateManagerMock(),
      DIContainer: diContainerMock(),
      deviceMgrKafkaTopics: global.config.devicemgr.kafka.consumer.topic.suffix,
      logger: new Logger('DeviceMgrEventEngine.test.js'),
    });
    deviceMgrEventEngine.logger.info = jest.fn();
    deviceMgrEventEngine.logger.error = jest.fn();
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  it('should initialize the DeviceMgrEventEngine', async () => {
    await expect(deviceMgrEventEngine.start()).resolves.toBeUndefined();
    expect(deviceMgrEventEngine.kafkaConsumer.init).toHaveBeenCalledTimes(1);
    expect(deviceMgrEventEngine.kafkaConsumer.registerCallback).toHaveBeenCalledTimes(1);
  });

  it('should stop the DeviceMgrEventEngine', async () => {
    await expect(deviceMgrEventEngine.stop()).resolves.toBeUndefined();
    expect(deviceMgrEventEngine.kafkaConsumer.finish).toHaveBeenCalledTimes(1);
  });

  it('should process the DeviceMgrEventEngine', async () => {
    const json = {};
    json.value = `{
      "meta": {
        "service": "admin"
      },
      "event": "create",
      "data": {
        "id": "abc123"
      }
    }`;
    await expect(deviceMgrEventEngine.process(json)).resolves.toBeUndefined();

    expect(deviceMgrEventEngine.stateManager.createBeacon).toHaveBeenCalledTimes(1);
    expect(deviceMgrEventEngine.stateManager.beacon.die).toHaveBeenCalledTimes(1);

    expect(deviceMgrEventEngine.container.createScope).toHaveBeenCalledTimes(1);
    expect(deviceMgrEventEngine.container.scope.register).toHaveBeenCalledTimes(1);
    expect(deviceMgrEventEngine.container.scope.resolve).toHaveBeenCalledTimes(1);
    expect(deviceMgrEventEngine.container.runnable.run).toHaveBeenCalledTimes(1);

    expect(deviceMgrEventEngine.container.scope.resolve('logger')).toBeInstanceOf(Logger);
  });

  it('should throw an exception when processing the DeviceMgrEventEngine', async () => {
    // simulates an exception when running Runnable
    deviceMgrEventEngine.container.runnable.run = jest.fn().mockRejectedValue(new Error('Async error'));

    const json = {};
    json.value = `{
      "meta": {
        "service": "admin"
      },
      "event": "remove",
      "data": {
        "id": "abc123"
      }
    }`;
    await expect(deviceMgrEventEngine.process(json)).resolves.toBeUndefined();

    expect(deviceMgrEventEngine.stateManager.createBeacon).toHaveBeenCalledTimes(1);
    expect(deviceMgrEventEngine.stateManager.beacon.die).toHaveBeenCalledTimes(1);

    expect(deviceMgrEventEngine.container.createScope).toHaveBeenCalledTimes(1);
    expect(deviceMgrEventEngine.container.scope.register).toHaveBeenCalledTimes(1);
    expect(deviceMgrEventEngine.container.scope.resolve).toHaveBeenCalledTimes(1);
    expect(deviceMgrEventEngine.container.runnable.run).toHaveBeenCalledTimes(1);

    expect(deviceMgrEventEngine.logger.error).toHaveBeenCalledTimes(1);

    deviceMgrEventEngine.container.runnable.run = jest.fn().mockResolvedValue(undefined);
  });
});
