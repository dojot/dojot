
const mockSignalReady = jest.fn();
const mockNotSignalReady = jest.fn();
const mockRegisterShutdownHandler = jest.fn();
const mockShutdown = jest.fn();

const mockSdk = {
  ServiceStateManager: {
    Manager: jest.fn().mockImplementation(() => ({
      signalReady: mockSignalReady,
      signalNotReady: mockNotSignalReady,
      registerShutdownHandler: mockRegisterShutdownHandler,
      shutdown: mockShutdown,
    })),
  },
};

jest.useFakeTimers();
jest.mock('@dojot/microservice-sdk', () => mockSdk);

const ServiceStateMgmt = require('../../app/ServiceStateMgmt');

describe('ServiceStateMgmt', () => {
  beforeAll(() => {
  });

  beforeEach(() => {
    jest.clearAllMocks();
  });

  afterAll(() => {
  });

  afterEach(() => {
  });

  test('check if init correct', () => {
    expect(ServiceStateMgmt.manager).toBeDefined();
  });


  test('addHealthChecker: signalReady', (done) => {
    // eslint-disable-next-line no-unused-vars
    const callback = (signalReady, signalNotReady) => {
      signalReady();
      done();
    };
    ServiceStateMgmt.addHealthChecker(callback, 10);
    jest.runOnlyPendingTimers();
    expect(setInterval).toHaveBeenLastCalledWith(expect.any(Function), 10);
    expect(setInterval).toHaveBeenCalledTimes(1);

    expect(mockSignalReady).toHaveBeenCalled();
  });

  test('addHealthChecker: signalNotReady', (done) => {
    const callback = (signalReady, signalNotReady) => {
      signalNotReady();
      done();
    };
    ServiceStateMgmt.addHealthChecker(callback, 10);
    jest.runOnlyPendingTimers();
    expect(setInterval).toHaveBeenLastCalledWith(expect.any(Function), 10);
    expect(setInterval).toHaveBeenCalledTimes(1);

    expect(mockNotSignalReady).toHaveBeenCalled();
  });

  test('registerShutdown: register', () => {
    ServiceStateMgmt.registerShutdown(() => {});
    expect(mockRegisterShutdownHandler).toHaveBeenCalled();
  });

  test('registerShutdown: shutdown', async () => {
    await ServiceStateMgmt.shutdown();
    expect(mockShutdown).toHaveBeenCalled();
  });
});
