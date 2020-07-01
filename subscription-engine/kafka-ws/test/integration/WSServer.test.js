const { WSServer } = require('../../app/WSServer');

jest.mock('@dojot/microservice-sdk');
jest.mock('ws');
jest.mock('uuid');
jest.mock('../../app/Errors');
jest.mock('../../app/Redis/RedisExpireMgmt');
jest.mock('../../app/Kafka/KafkaTopicsConsumerCallbacksMgmt');
jest.mock('../../app/Config.js', () => ({
  server: {
    jwt_header_auth: true,
  },
  kafka: { consumer: {} },
}));

const makeJwtToken = (tenant, expSeconds, user = 'test') => {
  const payload = {
    service: tenant,
    username: user,
    exp: expSeconds,
  };
  return `${Buffer.from('jwt schema').toString('base64')}.${
    Buffer.from(JSON.stringify(payload)).toString('base64')}.${
    Buffer.from('dummy signature').toString('base64')}`;
};

let wSServer = null;
describe('Testing WSServer - works fine', () => {
  beforeAll(() => {
    wSServer = new WSServer();
  });
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('Should init correctly ', async () => {
    let someError = false;
    try {
      await wSServer.init();
    } catch (e) {
      someError = true;
    }
    expect(someError).toBe(false);
  });

  it('Should onConnection ', async () => {
    const req = {
      connection: {
        remoteAddress: '1.1.1.1',
        remotePort: 80,
      },
      url: 'http://localhost:5000/api/v1/topics/tenant2.ws.example.test?fields=location&where=temperature=gte:20;',
      headers: {
        authorization: `Bearer ${makeJwtToken('tenant', 123)}`,
      },
    };

    const ws = {
      close: jest.fn(),
      send: jest.fn(),
      on: jest.fn(),
    };

    wSServer.onConnection(ws, req);

    expect(ws.close).toHaveBeenCalled();
    expect(ws.on).toHaveBeenCalled();
  });
});
