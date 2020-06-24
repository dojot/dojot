const redismock = require('redis-mock');
const RedisExpireMgmt = require('../../app/Redis/RedisExpireMgmt');

jest.mock('@dojot/microservice-sdk');
/* eslint-disable-next-line */
jest.mock('redis', () => require('redis-mock'));
jest.mock('../../app/Config.js', () => ({
  redis: {
    host: 'redis',
    port: 6379,
    database: 1,
  },
}));

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
    redisExpireMgmt.initSubscribe();
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
    redisExpireMgmt.initSubscribe();
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
    redisExpireMgmt.clients.pub.emit('error');
    redisExpireMgmt.clients.pub.connected = false;
  });

  it('Should emmit a error sub  ', (done) => {
    redisExpireMgmt.clients.sub.on('error', () => {
      done();
    });

    redisExpireMgmt.clients.sub.emit('error');
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
