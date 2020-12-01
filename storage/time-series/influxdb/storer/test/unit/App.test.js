const mockConfig = {
  lightship: { a: 'abc' },
  delete: { 'device.data.enable': true, 'tenant.data.enable': true },
};
const mockSdk = {
  ConfigManager: {
    getConfig: jest.fn(() => mockConfig),
    transformObjectKeys: jest.fn((obj) => obj),
  },
  ServiceStateManager:
    jest.fn().mockImplementation(() => ({
      registerService: jest.fn(),
    })),
  Logger: jest.fn(() => ({
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
  })),
};
jest.mock('@dojot/microservice-sdk', () => mockSdk);

const mockStateIsReady = jest.fn();
const mockCreateInfluxHealthChecker = jest.fn();
const mockInfluxRegisterShutdown = jest.fn();
const mockCreateOrgWithDefaultBucket = jest.fn();
const mockDeleteOrg = jest.fn();
const mockCloseOne = jest.fn();
const mockWrite = jest.fn();
const mockDeleteMeasurement = jest.fn();
const mockInflux = jest.fn().mockImplementation(() => ({
  createHealthChecker: mockCreateInfluxHealthChecker,
  registerShutdown: mockInfluxRegisterShutdown,
  getInfluxStateInstance: jest.fn().mockImplementation(() => ({
    isReady: mockStateIsReady,
  })),
  getInfluxOrgInstance: jest.fn().mockImplementation(() => ({
    createOrgWithDefaultBucket: mockCreateOrgWithDefaultBucket,
    deleteOrg: mockDeleteOrg,
  })),
  getInfluxDataWriterInstance: jest.fn().mockImplementation(() => ({
    write: mockWrite,
    closeAll: jest.fn(),
    closeOne: mockCloseOne,
  })),
  getInfluxMeasurementInstance: jest.fn().mockImplementation(() => ({
    deleteMeasurement: mockDeleteMeasurement,
  })),
}));
jest.mock('../../app/influx', () => mockInflux);

const mockKafkaHealthChecker = jest.fn();
const mockKafkaRegisterShutdown = jest.fn();
const mockKafkaConInit = jest.fn();
const mockKafkaRegisterCallbackForTenantEvents = jest.fn();
const mockKafkaRegisterCallbacksForDeviceDataEvents = jest.fn();
const mockKafkaRegisterCallbacksForDeviceMgmtEvents = jest.fn();
const mockKafka = jest.fn().mockImplementation(() => ({
  createHealthChecker: mockKafkaHealthChecker,
  getKafkaConsumerInstance: jest.fn().mockImplementation(() => ({
    init: mockKafkaConInit,
    registerCallbackForTenantEvents: mockKafkaRegisterCallbackForTenantEvents,
    registerCallbacksForDeviceDataEvents: mockKafkaRegisterCallbacksForDeviceDataEvents,
    registerCallbacksForDeviceMgmtEvents: mockKafkaRegisterCallbacksForDeviceMgmtEvents,
  })),
  registerShutdown: mockKafkaRegisterShutdown,
}));
jest.mock('../../app/kafka', () => mockKafka);

const App = require('../../app/App');

describe('App', () => {
  let app = null;
  beforeAll(() => {
    app = null;
  });

  beforeEach(() => {
    jest.clearAllMocks();
  });

  afterAll(() => {
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  test('instantiate class and init with error - influx not ready', async () => {
    expect.assertions(1);
    app = new App();
    mockStateIsReady.mockResolvedValueOnce(false);
    try {
      await app.init(() => {});
    } catch (e) {
      expect(e.message).toBe('Influxdb is not ready');
    }
  });

  test('instantiate class and init', async () => {
    app = new App();
    mockStateIsReady.mockResolvedValueOnce(true);
    await app.init();

    expect(mockCreateInfluxHealthChecker).toBeCalled();
    expect(mockInfluxRegisterShutdown).toBeCalled();

    expect(mockKafkaHealthChecker).toBeCalled();
    expect(mockKafkaRegisterShutdown).toBeCalled();

    expect(mockKafkaConInit).toBeCalled();

    expect(mockKafkaRegisterCallbackForTenantEvents).toBeCalled();

    const callbackCreateTenant = mockKafkaRegisterCallbackForTenantEvents.mock.calls[0][0];
    const callbackDeleteTenant = mockKafkaRegisterCallbackForTenantEvents.mock.calls[0][1];

    await callbackCreateTenant('tenant1');

    expect(mockCreateOrgWithDefaultBucket).toBeCalled();

    await callbackDeleteTenant('tenant1');

    expect(mockDeleteOrg).toBeCalledWith('tenant1');
    expect(mockCloseOne).toBeCalledWith('tenant1');

    const callbackWriteData = mockKafkaRegisterCallbacksForDeviceDataEvents.mock.calls[0][0];
    await callbackWriteData('tenant1', 'deviceid', 'attrs', 'timestamp');
    expect(mockWrite).toBeCalledWith('tenant1', 'deviceid', 'timestamp', 'attrs');

    const callbackWriteData2 = mockKafkaRegisterCallbacksForDeviceMgmtEvents.mock.calls[0][0];
    const callbackDeleteMeasurement = mockKafkaRegisterCallbacksForDeviceMgmtEvents
      .mock.calls[0][1];
    await callbackWriteData2('tenant2', 'deviceid2', 'attrs2', 'timestamp2');
    expect(mockWrite).toBeCalledWith('tenant2', 'deviceid2', 'timestamp2', 'attrs2');

    await callbackDeleteMeasurement('tenant2', 'deviceId2');
    expect(mockDeleteMeasurement).toBeCalledWith('tenant2', 'deviceId2');
  });
});
