const mockConfig = {
  redis: {
    host: 'redis',
    port: 6379,
    database: 1,
  },
};

const mockLogWarn = jest.fn();
const mockMicroServiceSdk = {
  ConfigManager: {
    getConfig: jest.fn(() => mockConfig),
    transformObjectKeys: jest.fn((obj) => obj),
  },
  Logger: jest.fn(() => ({
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    warn: mockLogWarn,
  })),
  ServiceStateManager: jest.fn(() => ({
    registerService: jest.fn(),
    signalReady: jest.fn(),
    signalNotReady: jest.fn(),
    addHealthChecker: jest.fn((service, callback) => callback(jest.fn(), jest.fn())),
    registerShutdownHandler: jest.fn(),
    shutdown: jest.fn().mockResolvedValue(),
  })),
};

jest.mock('@dojot/microservice-sdk', () => mockMicroServiceSdk);
jest.mock('../../app/StateManager');

const redismock = require('redis-mock');
const RedisExpireMgmt = require('../../app/Redis/RedisExpireMgmt');

/* eslint-disable-next-line */
jest.mock('redis', () => require('redis-mock'));

class ErrorTest extends Error {
  constructor(message, code) {
    super(message);
    this.code = code;
  }
}

let redisExpireMgmt = null;
let callbackTestXXX = null;
describe('Testing RedisExpireMgmt everything ok', () => {
  beforeAll(() => {
    redisExpireMgmt = new RedisExpireMgmt();
    callbackTestXXX = jest.fn();
  });
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('Should Connect pub  ', (done) => {
    redisExpireMgmt.clients.pub = redismock.createClient();
    redisExpireMgmt.initPublisher();
    redisExpireMgmt.clients.pub.on('connect', () => {
      done();
    });
  });

  it('Should Connect sub  ', (done) => {
    redisExpireMgmt.clients.sub = redismock.createClient();
    redisExpireMgmt.initSubscriber();
    redisExpireMgmt.clients.sub.on('connect', () => {
      done();
    });
  });

  it('Should Add a connection  ', () => {
    redisExpireMgmt.addConnection('xxx', 123, callbackTestXXX);
    expect(redisExpireMgmt.expirationMap.has('xxx')).toBe(true);
    expect(redisExpireMgmt.expirationMap.size).toBe(1);
  });

  it('Shouldnt add a connection that already exist  ', () => {
    expect(redisExpireMgmt.expirationMap.size).toBe(1);
    redisExpireMgmt.addConnection('xxx', 123, () => { });
    expect(redisExpireMgmt.expirationMap.has('xxx')).toBe(true);
    expect(redisExpireMgmt.expirationMap.size).toBe(1);
  });

  it('onMessage, callback', () => {
    redisExpireMgmt.onMessage('chan', 'xxx');
    expect(callbackTestXXX).toHaveBeenCalledTimes(1);
    redisExpireMgmt.onMessage('chan', 'notexist');
  });

  it('Should remove a connection  ', () => {
    redisExpireMgmt.removeConnection('xxx');
    expect(redisExpireMgmt.expirationMap.has('xxx')).toBe(false);
    expect(redisExpireMgmt.expirationMap.size).toBe(0);
  });

  it('Shouldnt remove a connection that not exist', () => {
    expect(redisExpireMgmt.expirationMap.size).toBe(0);
    redisExpireMgmt.removeConnection('xxx');
    expect(redisExpireMgmt.expirationMap.has('xxx')).toBe(false);
    expect(redisExpireMgmt.expirationMap.size).toBe(0);
  });

  it('Should end  ', (done) => {
    redisExpireMgmt.end();
    redisExpireMgmt.clients.sub.on('end', () => {
      done();
    });
  });
});

describe('Testing RedisExpireMgmt connect but has emit error', () => {
  beforeAll(() => {
    redisExpireMgmt = new RedisExpireMgmt();
    callbackTestXXX = jest.fn();
  });
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('Should Connect pub  ', (done) => {
    redisExpireMgmt.clients.pub = redismock.createClient();
    redisExpireMgmt.initPublisher();
    redisExpireMgmt.clients.pub.on('connect', () => {
      done();
    });
  });

  it('Should Connect sub  ', (done) => {
    redisExpireMgmt.clients.sub = redismock.createClient();
    redisExpireMgmt.initSubscriber();
    redisExpireMgmt.clients.sub.on('connect', () => {
      done();
    });
  });

  it('Shouldnt add a connection to test remove with connectin with redis', () => {
    redisExpireMgmt.addConnection('xxx', 123, callbackTestXXX);
    expect(redisExpireMgmt.expirationMap.has('xxx')).toBe(true);
    expect(redisExpireMgmt.expirationMap.size).toBe(1);
  });

  it('Should emmit a error pub  ', (done) => {
    redisExpireMgmt.clients.pub.on('error', () => {
      done();
    });
    redisExpireMgmt.clients.pub.emit('error', new ErrorTest('MSG', 'code'));
    redisExpireMgmt.clients.pub.connected = false;
  });

  it('Should emmit a error pub - excessive connection attempts ', (done) => {
    redisExpireMgmt.clients.pub.on('error', () => {
      expect(mockLogWarn).toHaveBeenCalledWith('The service will be shutdown for exceeding attempts to reconnect with Redis');
      done();
    });
    redisExpireMgmt.clients.pub.emit('error', new ErrorTest('MSG', 'CONNECTION_BROKEN'));
    redisExpireMgmt.clients.pub.connected = false;
  });

  it('Should emmit a error pub  ', (done) => {
    redisExpireMgmt.clients.sub.on('error', () => {
      done();
    });
    redisExpireMgmt.clients.sub.emit('error', new ErrorTest('MSG', 'code'));
    redisExpireMgmt.clients.sub.connected = false;
  });

  it('Should emmit a error sub - excessive connection attempts ', (done) => {
    redisExpireMgmt.clients.sub.on('error', () => {
      expect(mockLogWarn).toHaveBeenCalledWith('The service will be shutdown for exceeding attempts to reconnect with Redis');
      done();
    });
    redisExpireMgmt.clients.sub.emit('error', new ErrorTest('MSG', 'CONNECTION_BROKEN'));
    redisExpireMgmt.clients.sub.connected = false;
  });

  it('Shouldnt add a connection  when redis is not connect ', () => {
    redisExpireMgmt.addConnection('xxx2', 123, callbackTestXXX);
    expect(redisExpireMgmt.expirationMap.has('xxx2')).toBe(false);
  });

  it('Shouldnt remove a connection  when redis is not connect ', () => {
    redisExpireMgmt.removeConnection('xxx');
    expect(redisExpireMgmt.expirationMap.has('xxx')).toBe(true);
    expect(redisExpireMgmt.expirationMap.size).toBe(1);
  });

  it('Should emit warn', () => {
    redisExpireMgmt.clients.sub.emit('warning');
    redisExpireMgmt.clients.pub.emit('warning');
  });
});
