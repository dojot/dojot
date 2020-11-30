const mockConfig = {
  influx: {
    url: 'abc',
    'default.token': 'abc',
    'default.bucket': 'abc',
  },
};

const mockSdk = {
  ConfigManager: {
    getConfig: jest.fn(() => mockConfig),
  },
  Logger: jest.fn(() => ({
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
  })),
};
jest.mock('@dojot/microservice-sdk', () => mockSdk);

const mockStateIsReady = jest.fn();
const mockStateIsHealth = jest.fn();
const mockInfluxState = jest.fn().mockImplementation(() => ({
  isReady: mockStateIsReady,
  isHealth: mockStateIsHealth,
}));
jest.mock('../../app/influx/State', () => mockInfluxState);

const mockInfluxDataWriterWrite = jest.fn();

const mockInfluxDataWriterCloseAll = jest.fn();
const mockInfluxDataWriter = jest.fn().mockImplementation(() => ({
  write: mockInfluxDataWriterWrite,
  closeAll: mockInfluxDataWriterCloseAll,
}));
jest.mock('../../app/influx/DataWriter', () => mockInfluxDataWriter);

const mockInfluxInitOnboarding = jest.fn();
const mockInfluxOrg = jest.fn().mockImplementation(() => ({
  initOnboarding: mockInfluxInitOnboarding,
}));
jest.mock('../../app/influx/Organizations', () => mockInfluxOrg);

const mockInfluxDeleteMeasurement = jest.fn();
const mockInfluxMeasurements = jest.fn().mockImplementation(() => ({
  deleteMeasurement: mockInfluxDeleteMeasurement,
}));
jest.mock('../../app/influx/Measurements', () => mockInfluxMeasurements);


const mockFlat = {
  unflatten: jest.fn(),
  flatten: jest.fn(),
};


jest.mock('flat', () => mockFlat);
jest.mock('lodash.camelcase');

const Influx = require('../../app/influx');


const mockAddHealthChecker = jest.fn();
const mockRegisterShutdownHandler = jest.fn();
const serviceStateMock = {
  addHealthChecker: mockAddHealthChecker,
  registerShutdownHandler: mockRegisterShutdownHandler,
};

describe('Influx', () => {
  let influx = null;
  beforeAll(() => {
    influx = null;
  });

  beforeEach(() => {
    jest.clearAllMocks();
  });

  afterAll(() => {
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  test('instantiate class', () => {
    influx = new Influx(serviceStateMock);
  });


  test('getInfluxStateInstance', () => {
    influx.getInfluxStateInstance().isReady();
    expect(mockStateIsReady).toBeCalled();
  });

  test('getInfluxDataWriterInstance', () => {
    influx.getInfluxDataWriterInstance().write();
    expect(mockInfluxDataWriterWrite).toBeCalled();
  });

  test('getInfluxMeasurementInstance', () => {
    influx.getInfluxMeasurementInstance().deleteMeasurement();
    expect(mockInfluxDeleteMeasurement).toBeCalled();
  });

  test('getInfluxOrgInstance', () => {
    influx.getInfluxOrgInstance().initOnboarding();
    expect(mockInfluxInitOnboarding).toBeCalled();
  });

  test('createInfluxHealthChecker - not heath', async () => {
    influx.createHealthChecker();

    const callback = mockAddHealthChecker.mock.calls[0][1];
    mockStateIsHealth.mockResolvedValue(false);
    const ready = jest.fn();
    const notReady = jest.fn();
    await callback(ready, notReady);

    expect(mockAddHealthChecker).toHaveBeenCalled();
    expect(ready).not.toHaveBeenCalled();
    expect(notReady).toHaveBeenCalled();
  });

  test('createInfluxHealthChecker - heath', async () => {
    influx.createHealthChecker();

    const callback = mockAddHealthChecker.mock.calls[0][1];
    mockStateIsHealth.mockResolvedValue(true);
    const ready = jest.fn();
    const notReady = jest.fn();
    await callback(ready, notReady);

    expect(mockAddHealthChecker).toHaveBeenCalled();
    expect(ready).toHaveBeenCalled();
    expect(notReady).not.toHaveBeenCalled();
  });

  test('registerShutdown', async () => {
    await influx.registerShutdown();

    const callback = mockRegisterShutdownHandler.mock.calls[0][0];
    await callback();

    expect(mockRegisterShutdownHandler).toHaveBeenCalled();
    expect(mockInfluxDataWriterCloseAll).toHaveBeenCalled();
  });
});
