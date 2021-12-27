const mockConfig = {
  redis: {
    host: 'acl-redis',
    port: 6379,
    db: 0,
    'reconnect.after.ms': 5000,
    'operation.timeout.ms': 1000,
  },
};

jest.mock('@dojot/microservice-sdk');

const sdkMock = require('@dojot/microservice-sdk');

sdkMock.ConfigManager.getConfig = jest.fn(() => mockConfig);

const { ServiceStateManager } = require('@dojot/microservice-sdk');

const stateManagerMock = new ServiceStateManager();
// registerShutdownHandler and shutdown - defined inside a constructor
stateManagerMock.registerShutdownHandler = jest.fn();
stateManagerMock.shutdown = jest.fn();

function MockRedisClient() {
  this.eventListener = {};
  this.table = new Map();
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
MockRedisClient.prototype.set = function set(
  key, value, cb,
) {
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

const RedisManager = require('../../app/redis/RedisManager');

describe('Redis Initialization', () => {
  it('Constructor', () => {
    const redisManager = new RedisManager(stateManagerMock);

    expect(redisManager.eventEmitter).toBeDefined();
    expect(stateManagerMock.registerShutdownHandler).toBeCalled();
  });
});

describe('Set/Delete', () => {
  it('Set data successfully', async () => {
    const redisManager = new RedisManager(stateManagerMock);

    const ret = await redisManager.setAsync('fingerprintX', 'tenant:deviceId');
    expect(ret).toBe('OK');
  });

  it('Delete data successfully', async () => {
    const redisManager = new RedisManager(stateManagerMock);
    await redisManager.setAsync('fingerprintX', 'tenant:deviceId');
    await redisManager.setAsync('fingerprintY', 'application');

    const ret1 = await redisManager.delAsync('fingerprintX');
    expect(ret1).toBe(1);
    const ret2 = await redisManager.delAsync('fingerprintY');
    expect(ret2).toBe(1);
    const ret3 = await redisManager.delAsync('fingerprintZ');
    expect(ret3).toBe(0);
  });
});

describe('Redis Events', () => {
  it('connect', () => {
    const redisManager = new RedisManager(stateManagerMock);
    redisManager.redisClient.emit('connect');

    expect(stateManagerMock.signalReady).toBeCalled();
    expect(redisManager.eventEmitter.emit).toBeCalledWith('healthy');
  });

  it('reconnecting', () => {
    const redisManager = new RedisManager(stateManagerMock);
    redisManager.redisClient.emit('reconnecting');

    expect(stateManagerMock.signalNotReady).toBeCalled();
    expect(redisManager.eventEmitter.emit).toBeCalledWith('unhealthy');
  });

  it('error - exhausted retries', () => {
    const redisManager = new RedisManager(stateManagerMock);
    redisManager.redisClient.emit('error', { code: 'CONNECTION_BROKEN' });

    expect(stateManagerMock.shutdown).toBeCalled();
  });

  it('error - except exhausted retries', () => {
    const redisManager = new RedisManager(stateManagerMock);
    redisManager.redisClient.emit('error',
      { code: 'ANY ERROR, EXCEPT CONNECTION_BROKEN' });

    expect(stateManagerMock.shutdown).not.toBeCalled();
  });

  it('end', () => {
    const redisManager = new RedisManager(stateManagerMock);
    redisManager.redisClient.emit('end');

    expect(stateManagerMock.signalNotReady).toBeCalled();
    expect(redisManager.eventEmitter.emit).toBeCalledWith('unhealthy');
  });
});

describe('Register on event', () => {
  it('healthy', () => {
    const redisManager = new RedisManager(stateManagerMock);
    const fn = jest.fn();
    redisManager.on('healthy', fn);

    expect(redisManager.eventEmitter.addListener)
      .toBeCalledWith('healthy', fn);
  });

  it('unhealthy', () => {
    const redisManager = new RedisManager(stateManagerMock);
    const fn = jest.fn();
    redisManager.on('unhealthy', fn);

    expect(redisManager.eventEmitter.addListener)
      .toBeCalledWith('unhealthy', fn);
  });

  it('invalid event', () => {
    const redisManager = new RedisManager(stateManagerMock);
    const fn = jest.fn();

    expect(() => redisManager.on('invalid event', fn)).toThrow();
  });
});

describe('Graceful Shutdown', () => {
  it('Finish Redis Client', async () => {
    const redisManager = new RedisManager(stateManagerMock);

    await redisManager.shutdown();

    expect(redisManager.redisClient.quit).toBeCalled();
  });
});
