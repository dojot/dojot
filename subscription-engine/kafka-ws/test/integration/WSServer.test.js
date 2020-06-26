const websocketTarball = require('../../app/WebsocketTarball');

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

describe('Testing WSServer - works fine', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('Should init correctly ', async () => {
    let someError = false;
    try {
      await websocketTarball.init();
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
      headers: {
        authorization: `Bearer ${makeJwtToken('tenant', 123)}`,
      },
    };

    const ws = {
      close: jest.fn(),
      send: jest.fn(),
      on: jest.fn(),
    };

    const topic = 'tenant2.ws.example.test';
    const fields = 'location';
    const where = 'temperature=gte:20;';
    websocketTarball.onConnection(ws, req, topic, fields, where);

    expect(ws.close).toHaveBeenCalled();
    expect(ws.on).toHaveBeenCalled();
  });
});
