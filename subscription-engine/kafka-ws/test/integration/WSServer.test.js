const websocketTarball = require('../../app/WebsocketTarball');

jest.mock('@dojot/microservice-sdk');
jest.mock('ws');
jest.mock('uuid');
jest.mock('../../app/Errors');
jest.mock('../../app/Redis/RedisExpireMgmt');
jest.mock('../../app/Kafka/KafkaTopicsConsumerCallbacksMgmt');
jest.mock('../../app/Config.js', () => ({
  server: {
    requireTicket: true,
  },
  kafka: { consumer: {} },
}));

const makeJwtToken = (tenant, iatSeconds, expSeconds, user = 'test') => {
  const payload = {
    service: tenant,
    username: user,
    iat: iatSeconds,
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
      headers: {
        authorization: `Bearer ${makeJwtToken('tenant', 123, 130)}`,
      },
      connection: {
        remoteAddress: '1.1.1.1',
        remotePort: 80,
      },
      token: {
        tenant: 'tenant',
        remainingTime: 130,
      },
      params: {
        topic: 'tenant2.ws.example.test',
      },
      query: {
        fields: 'location',
        where: 'temperature=gte:20;',
      },
    };

    const ws = {
      close: jest.fn(),
      send: jest.fn(),
      on: jest.fn(),
    };

    const params = {
      ws,
      connection: req.connection,
      token: req.token,
      topic: req.params.topic,
      fields: req.query.fields,
      where: req.query.where,
    };

    websocketTarball.onConnection(params);

    expect(ws.close).toHaveBeenCalled();
    expect(ws.on).toHaveBeenCalled();
  });
});
