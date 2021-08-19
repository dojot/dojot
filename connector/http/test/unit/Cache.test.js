/* eslint-disable jest/no-conditional-expect */
/* eslint-disable jest/no-try-expect */
const mockCacheGet = jest.fn();
const mockCacheSet = jest.fn();
const mockCacheGetStats = jest.fn();
const mockCacheClose = jest.fn();
const mockCache = {
  get: mockCacheGet,
  set: mockCacheSet,
  getStats: mockCacheGetStats,
  close: mockCacheClose,
};
const mockNodeCache = jest.fn().mockImplementation(() => mockCache);
jest.mock('node-cache', () => mockNodeCache);

const mockAddHealthChecker = jest.fn();
const mockRegisterShutdownHandler = jest.fn();
const mockSignalReady = jest.fn();
const mockSignalNotReady = jest.fn();
const serviceStateMock = {
  addHealthChecker: mockAddHealthChecker,
  registerShutdownHandler: mockRegisterShutdownHandler,
  signalReady: mockSignalReady,
  signalNotReady: mockSignalNotReady,
};

const mockConfig = {
  cache: { 'std.tll': 100, checkperiod: 120 },
  healthchecker: { 'kafka.interval.ms': 30000 },
};

jest.mock('@dojot/microservice-sdk', () => ({
  ConfigManager: { getConfig: jest.fn(() => mockConfig) },
  Logger: jest.fn(() => ({
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
  })),
}));

const { killApplication } = require('../../app/Utils');

jest.mock('../../app/Utils');

const Cache = require('../../app/Cache');

describe('Cache', () => {
  let cache;

  beforeEach(async () => {
    cache = new Cache(serviceStateMock);
  });

  afterAll(() => {
    jest.clearAllMocks();
  });

  describe('constructor', () => {
    it('should successfully create a new instance', () => {
      expect(cache.serviceState).toEqual(serviceStateMock);
      expect(cache.myCache).toBeDefined();
    });
  });

  describe('init', () => {
    beforeEach(() => {
      jest.clearAllMocks();
    });

    it('should correctly initialize', () => {
      cache.init();
      expect(mockNodeCache).toHaveBeenCalled();
    });

    it('should return error and kill process', () => {
      mockNodeCache.mockImplementationOnce(() => {
        throw new Error('Test');
      });
      cache.init();
      expect(killApplication).toHaveBeenCalled();
    });
  });

  describe('get', () => {
    const key = 'T1:E2:S3:T4';
    it('should get value', async () => {
      cache.init();
      cache.get(key);
      expect(cache.myCache.get).toHaveBeenCalled();
    });
  });

  describe('set', () => {
    const key = 'T1:E2:S3:T4';
    it('should set value', async () => {
      cache.init();
      cache.set(key);
      expect(cache.myCache.set).toHaveBeenCalled();
    });
  });

  describe('healthChecker', () => {
    let signalReady;
    let signalNotReady;

    beforeEach(async () => {
      jest.clearAllMocks();
      signalReady = jest.fn();
      signalNotReady = jest.fn();
      cache.init();
    });

    it('should signal as ready - is connected to Kafka', async () => {
      cache.myCache.getStats.mockReturnValue({ misses: 1 });

      cache.createHealthChecker();

      const callback = mockAddHealthChecker.mock.calls[0][1];
      await callback(signalReady, signalNotReady);

      expect(mockAddHealthChecker).toHaveBeenCalled();
      expect(signalNotReady).not.toHaveBeenCalled();
      expect(signalReady).toHaveBeenCalled();
    });

    it('should signal as not ready - is not connected to Kafka', async () => {
      cache.myCache.getStats.mockReturnValue({ misses: 0 });

      cache.createHealthChecker();

      const callback = mockAddHealthChecker.mock.calls[0][1];
      await callback(signalReady, signalNotReady);

      expect(mockAddHealthChecker).toHaveBeenCalled();
      expect(signalReady).not.toHaveBeenCalled();
      expect(signalNotReady).toHaveBeenCalled();
    });

    it('should signal as not ready - a new Cache instance is not correctly created', async () => {
      cache.createHealthChecker();

      cache.myCache = undefined;

      const callback = mockAddHealthChecker.mock.calls[0][1];
      await callback(signalReady, signalNotReady);

      expect(mockAddHealthChecker).toHaveBeenCalled();
      expect(signalReady).not.toHaveBeenCalled();
      expect(signalNotReady).toHaveBeenCalled();
    });
  });

  describe('shutdownHandler', () => {
    it('should call the disconnect function from the cache', async () => {
      await cache.init();
      await cache.registerShutdown();
      const callback = mockRegisterShutdownHandler.mock.calls[0][0];
      callback();
      expect(mockRegisterShutdownHandler).toHaveBeenCalled();
    });
  });
});
