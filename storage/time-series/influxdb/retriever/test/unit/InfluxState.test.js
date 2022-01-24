const mockSdk = {
  Logger: jest.fn(() => ({
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
  })),
  ConfigManager: {
    getConfig: jest.fn(() => ({
      influx: {
        'heathcheck.ms': 3000,
      },
    })),
  },
};
jest.mock('@dojot/microservice-sdk', () => mockSdk);
jest.mock('@influxdata/influxdb-client');

const mockGetHealth = jest.fn();
const mockGetReady = jest.fn();
const mockInfluxApi = {
  HealthAPI: jest.fn().mockImplementation(() => ({
    getHealth: mockGetHealth,
  })),
  ReadyAPI: jest.fn().mockImplementation(() => ({
    getReady: mockGetReady,
  })),
};
jest.mock('@influxdata/influxdb-client-apis', () => mockInfluxApi);

const mockAddHealthChecker = jest.fn();
const serviceStateMock = {
  addHealthChecker: mockAddHealthChecker,
};


const State = require('../../app/influx/State');

describe('Test influx state', () => {
  let state = null;
  beforeAll(() => {
    state = null;
  });

  beforeEach(() => {
    jest.clearAllMocks();
    state = new State({}, serviceStateMock);
  });

  afterAll(() => {
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  test('Heath - true', async () => {
    mockGetHealth.mockResolvedValueOnce({ status: 'pass' });
    expect(await state.isHealth()).toBe(true);
  });

  test('Heath - false', async () => {
    mockGetHealth.mockResolvedValueOnce({ status: 'any' });
    expect(await state.isHealth()).toBe(false);
  });
  test('Heath - true - reject', async () => {
    mockGetHealth.mockRejectedValueOnce(new Error());
    expect(await state.isHealth()).toBe(false);
  });

  test('Ready - true', async () => {
    mockGetReady.mockResolvedValueOnce(({ status: 'ready' }));
    expect(await state.isReady()).toBe(true);
  });

  test('Ready - false', async () => {
    mockGetReady.mockResolvedValueOnce(({ status: 'any' }));
    expect(await state.isReady()).toBe(false);
  });
  test('Ready - true - reject', async () => {
    mockGetReady.mockRejectedValueOnce(new Error());
    expect(await state.isReady()).toBe(false);
  });

  test('createInfluxHealthChecker - heath', async () => {
    state.createInfluxHealthChecker();
    state.isHealth = jest.fn(() => true);
    const callback = mockAddHealthChecker.mock.calls[0][1];

    const ready = jest.fn();
    const notReady = jest.fn();
    await callback(ready, notReady);

    expect(mockAddHealthChecker).toHaveBeenCalled();
    expect(ready).toHaveBeenCalled();
    expect(notReady).not.toHaveBeenCalled();
  });

  test('createInfluxHealthChecker - not heath', async () => {
    state.createInfluxHealthChecker();
    state.isHealth = jest.fn(() => false);
    const callback = mockAddHealthChecker.mock.calls[0][1];

    const ready = jest.fn();
    const notReady = jest.fn();
    await callback(ready, notReady);

    expect(mockAddHealthChecker).toHaveBeenCalled();
    expect(ready).not.toHaveBeenCalled();
    expect(notReady).toHaveBeenCalled();
  });
});
