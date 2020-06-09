
const RedisExpireMgmt = require('../../app/Expiration/RedisExpireMgmt');
const redismock = require("redis-mock");
jest.mock('@dojot/microservice-sdk');
jest.mock('redis', () => require("redis-mock"))
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

    it('Should Connect pub  ', () => {
        redisExpireMgmt.clients.pub = redismock.createClient();
        redisExpireMgmt.initPublisher();
        redisExpireMgmt.clients.pub.emit('connect');
        expect(redisExpireMgmt.connect.pub).toBe(true)

    });

    it('Should Connect sub  ', () => {
        redisExpireMgmt.clients.sub = redismock.createClient();
        redisExpireMgmt.initSubscribe();
        redisExpireMgmt.clients.sub.emit('connect');
        expect(redisExpireMgmt.connect.sub).toBe(true)
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

    it('Should end  ', () => {
        redisExpireMgmt.end();
        redisExpireMgmt.clients.sub.emit('end');
        expect(redisExpireMgmt.connect.sub).toBe(false)
        redisExpireMgmt.clients.pub.emit('end');
        expect(redisExpireMgmt.connect.pub).toBe(false)
    });



});

describe('Testing RedisExpireMgmt not connect', () => {
    beforeAll(() => {
        redisExpireMgmt = new RedisExpireMgmt();
        callbackTestXXX = jest.fn();

    });
    beforeEach(() => {
        jest.clearAllMocks();
    });

    it('Shouldnt Connect pub  ', () => {
        redisExpireMgmt.clients.pub = redismock.createClient();
        redisExpireMgmt.initPublisher();
        redisExpireMgmt.clients.pub.emit('connect', new Error("x"));
        expect(redisExpireMgmt.connect.pub).toBe(false)
    });

    it('Shouldnt Connect sub  ', () => {
        redisExpireMgmt.clients.sub = redismock.createClient();
        redisExpireMgmt.initSubscribe();
        redisExpireMgmt.clients.sub.emit('connect', new Error("x"));
        expect(redisExpireMgmt.connect.sub).toBe(false)
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

    it('Should Connect pub  ', () => {
        redisExpireMgmt.clients.pub = redismock.createClient();
        redisExpireMgmt.initPublisher();
        redisExpireMgmt.clients.pub.emit('connect');
        expect(redisExpireMgmt.connect.pub).toBe(true)

    });

    it('Should Connect sub  ', () => {
        redisExpireMgmt.clients.sub = redismock.createClient();
        redisExpireMgmt.initSubscribe();
        redisExpireMgmt.clients.sub.emit('connect');
        expect(redisExpireMgmt.connect.sub).toBe(true)
    });

    it('Shouldnt add a connection to test remove with connectin with redis', () => {
        redisExpireMgmt.addConnection('xxx', 123, callbackTestXXX);
        expect(redisExpireMgmt.expirationMap.has('xxx')).toBe(true);
        expect(redisExpireMgmt.expirationMap.size).toBe(1);
    });

    it('Should emmit a error pub  ', () => {
        redisExpireMgmt.clients.pub.emit('error');
        expect(redisExpireMgmt.connect.pub).toBe(false)

    });

    it('Should emmit a error sub  ', () => {
        redisExpireMgmt.clients.sub.emit('error');
        expect(redisExpireMgmt.connect.sub).toBe(false)
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
        redisExpireMgmt.clients.pub.emit('warning');;
    });
});


