const mockConfig = {
  server: {

  },
};

const mockMicroServiceSdk = {
  ConfigManager: {
    getConfig: jest.fn(() => mockConfig),
    transformObjectKeys: jest.fn((obj) => obj),
  },
  Kafka: {
    Consumer: jest.fn(),
  },
  ServiceStateManager: {
    Manager: jest.fn(),
  },
  Logger: jest.fn(() => ({
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
  })),
};

jest.mock('@dojot/microservice-sdk', () => mockMicroServiceSdk);

jest.mock('ws');
jest.mock('uuid');
jest.mock('../../app/Errors');
jest.mock('../../app/Redis/RedisExpireMgmt');
jest.mock('../../app/Kafka/KafkaTopicsConsumerCallbacksMgmt');

const websocketTarball = require('../../app/WebsocketTarball');

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
