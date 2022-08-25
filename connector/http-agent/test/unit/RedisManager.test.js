/* eslint-disable security/detect-object-injection */

function MockRedisClient() {
  this.eventListener = {};
  this.table = new Map();
  this.getAsync = jest.fn();
  this.setAsync = jest.fn();
  this.quitAsync = jest.fn();
  this.quitAsync = jest.fn();
  this.delAsync = jest.fn();
}
MockRedisClient.prototype.emit = function emit(event, data) {
  this.eventListener[event](data);
  return this;
};
MockRedisClient.prototype.on = function on(event, cb) {
  this.eventListener[event] = cb;
  return this;
};
MockRedisClient.prototype.get = function get(key, cb) {
  const value = this.table.get(key);
  cb(null, value);
};
MockRedisClient.prototype.set = function set(key, value, cb) {
  this.table.set(key, value);
  cb(null, 'OK');
};
MockRedisClient.prototype.del = function del(key, cb) {
  const ret = this.table.delete(key);
  cb(null, (ret) ? 1 : 0);
};
MockRedisClient.prototype.quit = jest.fn((cb) => {
  cb();
});
const mockRedis = {
  RedisClient: MockRedisClient,
  createClient: jest.fn(() => new MockRedisClient()),
};

jest.mock('redis', () => mockRedis);

jest.mock('events');

const mockRegisterShutdownHandler = jest.fn();
const mockSignalReady = jest.fn();
const mockSignalNotReady = jest.fn();
const mockShutdown = jest.fn();
const serviceStateMock = {
  registerShutdownHandler: mockRegisterShutdownHandler,
  signalReady: mockSignalReady,
  signalNotReady: mockSignalNotReady,
  shutdown: mockShutdown,
};

const mockConfig = {
  redis: {
    host: 'acl-redis',
    port: 6379,
    db: 0,
    'reconnect.after.ms': 5000,
    'operation.timeout.ms': 1000,
  },
};

const { killApplication } = require('../../app/Utils');

jest.mock('../../app/Utils');

jest.mock('@dojot/microservice-sdk', () => ({
  ConfigManager: { getConfig: jest.fn(() => mockConfig) },
  Logger: jest.fn(() => ({
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
  })),
}));

jest.mock('util');

jest.mock('../../app/Utils');

const RedisManager = require('../../app/redis/RedisManager');

describe('RedisManager', () => {
  let redisManager;

  beforeEach(async () => {
    redisManager = new RedisManager(serviceStateMock);
  });

  afterAll(() => {
    jest.clearAllMocks();
  });

  describe('constructor', () => {
    it('should successfully create a new instance', () => {
      expect(redisManager.serviceState).toEqual(serviceStateMock);
      expect(redisManager.redisClient).toBeDefined();
      expect(redisManager.eventEmitter).toBeDefined();
    });
  });


  describe('Redis Events', () => {
    beforeEach(() => {
      jest.clearAllMocks();
      redisManager.init();
    });

    it('connect', () => {
      redisManager.redisClient.eventListener.connect();

      expect(serviceStateMock.signalReady).toHaveBeenCalledWith(
        'http-redis',
      );
    });

    it('reconnecting', () => {
      redisManager.redisClient.eventListener.reconnecting();

      expect(serviceStateMock.signalNotReady).toHaveBeenCalledWith(
        'http-redis',
      );
    });

    it('error - exhausted retries', () => {
      redisManager.redisClient.emit('error', { code: 'CONNECTION_BROKEN' });

      expect(serviceStateMock.shutdown).toBeCalled();
    });

    it('error - except exhausted retries', () => {
      redisManager.redisClient.emit(
        'error',
        { code: 'ANY ERROR, EXCEPT CONNECTION_BROKEN' },
      );

      expect(serviceStateMock.shutdown).not.toBeCalled();
    });

    it('end', () => {
      redisManager.redisClient.emit('end');

      expect(serviceStateMock.signalNotReady).toBeCalled();
      expect(redisManager.eventEmitter.emit).toBeCalledWith('unhealthy');
    });
  });

  describe('Init', () => {
    it('should return error and kill process', () => {
      mockRedis.createClient.mockImplementationOnce(() => {
        throw new Error('Test');
      });
      redisManager.init();
      expect(killApplication).toHaveBeenCalled();
    });
  });

  beforeEach(() => {
    jest.clearAllMocks();
    redisManager.init();
  });

  describe('getAsync', () => {
    const key = 'T1:E2:S3:T4';
    it('should get value', async () => {
      redisManager.getAsync(key);
      expect(redisManager.redisClient.getAsync).toHaveBeenCalled();
    });
  });

  describe('setAsync', () => {
    const key = 'T1:E2:S3:T4';
    const value = 'test:123abc';
    it('should set value', async () => {
      redisManager.setAsync(key, value);
      expect(redisManager.redisClient.setAsync).toHaveBeenCalled();
    });
  });

  describe('getSecurity', () => {
    const key = 'test@123abc';
    it('should get value', async () => {
      redisManager.redisClient.getAsync.mockReturnValue('$ioaswru9834%34r7rfnedfv');
      redisManager.getSecurity(key);
      expect(redisManager.redisClient.getAsync).toHaveBeenCalled();
    });

    it('should not get value', async () => {
      redisManager.redisClient.getAsync.mockReturnValue(null);
      redisManager.getSecurity(key);
      expect(redisManager.redisClient.getAsync).toHaveBeenCalled();
    });
  });

  describe('setSecurity', () => {
    const key = 'test@123abc';
    const value = 'test:123abc';
    it('should set value', async () => {
      redisManager.setSecurity(key, value);
      expect(redisManager.redisClient.setAsync).toHaveBeenCalled();
    });
  });

  describe('reconnectAfter', () => {
    it('return the integer with reconnection time', async () => {
      const reconnectAfterSpy = jest.spyOn(RedisManager, 'reconnectAfter');
      const reconnectAfterMS = reconnectAfterSpy();
      expect(reconnectAfterMS).toEqual(mockConfig.redis['reconnect.after.ms']);
    });
  });

  describe('shutdownHandler', () => {
    it('should call the disconnect function from the redisManager', async () => {
      await redisManager.registerShutdown();
      const callback = mockRegisterShutdownHandler.mock.calls[0][0];
      callback();
      expect(mockRegisterShutdownHandler).toHaveBeenCalled();
    });
  });
});
