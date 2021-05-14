const mockConfig = {
  redis: {
    host: 'acl-redis',
    port: 6379,
    db: 0,
    'reconnect.after.ms': 5000,
  },
};

jest.mock('@dojot/microservice-sdk');
const sdkMock = require('@dojot/microservice-sdk');

sdkMock.ConfigManager.getConfig = jest.fn(() => mockConfig);

jest.mock('../../app/StateManager');
const stateManagerMock = require('../../app/StateManager');
// registerShutdownHandler and shutdown - defined inside a constructor
stateManagerMock.registerShutdownHandler = jest.fn();
stateManagerMock.shutdown = jest.fn();

class MockRedisClient {
  constructor() {
    this.eventListener = {};
    this.emit = (event, data) => {
      this.eventListener[event](data);
    };
    this.on = jest.fn((event, cb) => {
      this.eventListener[event] = cb;
    });
    this.table = new Map();
    this.set = jest.fn((key, value, cb) => {
      this.table.set(key, value);
      cb(null, 'OK');
    });
    this.del = jest.fn((key, cb) => {
      const ret = this.table.delete(key);
      cb(null, (ret) ? 1 : 0);
    });
    this.quit = jest.fn((cb) => cb());
  }
}
const mockRedis = {
  createClient: jest.fn(() => new MockRedisClient()),
};

jest.mock('redis', () => mockRedis);

jest.mock('events');

const RedisManager = require('../../app/redis/RedisManager');

describe('Redis Initialization', () => {
  it('Constructor', () => {
    const redisManager = new RedisManager();

    expect(redisManager.eventEmitter).toBeDefined();
    expect(redisManager.setAsync).toBeDefined();
    expect(redisManager.delAsync).toBeDefined();
    expect(stateManagerMock.registerShutdownHandler).toBeCalled();
  });
});

describe('Set/Delete', () => {
  it('Set data successfully', () => {
    const redisManager = new RedisManager();

    expect(redisManager.setAsync('fingerprintX', 'tenant:deviceId'))
      .resolves.toBe('OK');
    expect(redisManager.setAsync('fingerprintY', 'application'))
      .resolves.toBe('OK');
  });

  it('Delete data successfully', () => {
    const redisManager = new RedisManager();
    redisManager.setAsync('fingerprintX', 'tenant:deviceId');
    redisManager.setAsync('fingerprintY', 'application');

    expect(redisManager.delAsync('fingerprintX'))
      .resolves.toBe(1);
    expect(redisManager.delAsync('fingerprintY'))
      .resolves.toBe(1);
    expect(redisManager.delAsync('fingerprintZ'))
      .resolves.toBe(0);
  });
});

describe('Redis Events', () => {
  it('connect', () => {
    const redisManager = new RedisManager();
    redisManager.redisClient.emit('connect');

    expect(stateManagerMock.signalReady).toBeCalled();
    expect(redisManager.eventEmitter.emit).toBeCalledWith('healthy');
  });

  it('reconnecting', () => {
    const redisManager = new RedisManager();
    redisManager.redisClient.emit('reconnecting');

    expect(stateManagerMock.signalNotReady).toBeCalled();
    expect(redisManager.eventEmitter.emit).toBeCalledWith('unhealthy');
  });

  it('error - exhausted retries', () => {
    const redisManager = new RedisManager();
    redisManager.redisClient.emit('error', { code: 'CONNECTION_BROKEN' });

    expect(stateManagerMock.shutdown).toBeCalled();
  });

  it('error - except exhausted retries', () => {
    const redisManager = new RedisManager();
    redisManager.redisClient.emit('error',
      { code: 'ANY ERROR, EXCEPT CONNECTION_BROKEN' });

    expect(stateManagerMock.shutdown).not.toBeCalled();
  });

  it('end', () => {
    const redisManager = new RedisManager();
    redisManager.redisClient.emit('end');

    expect(stateManagerMock.signalNotReady).toBeCalled();
    expect(redisManager.eventEmitter.emit).toBeCalledWith('unhealthy');
  });
});

describe('Register on event', () => {
  it('healthy', () => {
    const redisManager = new RedisManager();
    const fn = jest.fn();
    redisManager.on('healthy', fn);

    expect(redisManager.eventEmitter.addListener)
      .toBeCalledWith('healthy', fn);
  });

  it('unhealthy', () => {
    const redisManager = new RedisManager();
    const fn = jest.fn();
    redisManager.on('unhealthy', fn);

    expect(redisManager.eventEmitter.addListener)
      .toBeCalledWith('unhealthy', fn);
  });

  it('invalid event', () => {
    const redisManager = new RedisManager();
    const fn = jest.fn();

    expect(() => redisManager.on('invalid event', fn)).toThrow();
  });
});

describe('Graceful Shutdown', () => {
  it('Finish Redis Client', async () => {
    const redisManager = new RedisManager();

    await redisManager.shutdown();

    expect(redisManager.redisClient.quit).toBeCalled();
  });
});
