const { Logger } = require('@dojot/microservice-sdk');

const TenantManagerEventEngine = require('../../../src/tenantManager/TenantManagerEventEngine');

const mockTenantManagerKafkaConsumer = {
  init: jest.fn().mockResolvedValue(undefined),
  finish: jest.fn().mockResolvedValue(undefined),
  registerCallback: jest.fn(),
};


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
    resolve: jest.fn((entry) => ({
      run: jest.fn(),
    })),
  };

  const diContainer = {
    createScope: jest.fn(() => scope),
    scope,
    runnable: deviceMgrEventRunnable,
  };
  return diContainer;
}

describe("Unit tests of script 'TenantManagerEventEngine.js'", () => {
  let deviceMgrEventEngine = null;

  beforeAll(() => {
    tenantManagerEventEngine = new TenantManagerEventEngine({
      kafkaConsumer: mockTenantManagerKafkaConsumer,
      tenantManagerKafkaTopics: 'topic',
      logger: new Logger('DeviceMgrEventEngine.test.js'),
      stateManager: stateManagerMock(),
      DIContainer: diContainerMock(),
    });
    tenantManagerEventEngine.logger.info = jest.fn();
    tenantManagerEventEngine.logger.error = jest.fn();
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  it('should initialize the TenantManagerEventEngine', async () => {
    await expect(tenantManagerEventEngine.start()).resolves.toBeUndefined();
    expect(tenantManagerEventEngine.kafkaConsumer.init).toHaveBeenCalledTimes(1);
    expect(tenantManagerEventEngine.kafkaConsumer.registerCallback).toHaveBeenCalledTimes(1);
  });

  it('should stop the TenantManagerEventEngine', async () => {
    await expect(tenantManagerEventEngine.stop()).resolves.toBeUndefined();
    expect(tenantManagerEventEngine.kafkaConsumer.finish).toHaveBeenCalledTimes(1);
  });

  it('should process the TenantManagerEventEngine', async () => {
    const json = {};
    json.value = `{
      "type": "CREATE",
      "tenant": {
        "id": "teste1",
        "signatureKey": {
          "certificate": "certificate",
          "algorithm": "RS256"
        }
      }
    }`;

    await expect(tenantManagerEventEngine.process(json)).resolves.toBeUndefined();

    expect(tenantManagerEventEngine.stateManager.createBeacon).toHaveBeenCalledTimes(1);
    expect(tenantManagerEventEngine.stateManager.beacon.die).toHaveBeenCalledTimes(1);

    expect(tenantManagerEventEngine.container.createScope).toHaveBeenCalledTimes(1);
    expect(tenantManagerEventEngine.container.scope.register).toHaveBeenCalledTimes(1);
    expect(tenantManagerEventEngine.container.scope.resolve).toHaveBeenCalledTimes(1);
  });
});
