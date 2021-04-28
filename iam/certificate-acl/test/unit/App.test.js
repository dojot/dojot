const mockConfig = {
  kafka: {},
  consumer: {
    'group.id': 'kafka_test',
    'metadata.broker.list': 'kafka:9092',
  },
  topic: {
    'auto.offset.reset': 'earliest',
  },
  healthcheck: {
    'kafka.interval.ms': 5000,
  },
  redis: {
    host: 'acl-redis',
    port: 6379,
    db: 0,
    'reconnect.after.ms': 5000,
  },
  app: {
    // eslint-disable-next-line no-useless-escape
    'consumer.topic': '^.*dojot\.x509-identity-mgmt\.certificates',
  },
};

jest.mock('@dojot/microservice-sdk');
const sdkMock = require('@dojot/microservice-sdk');

sdkMock.ConfigManager.getConfig = jest.fn(() => mockConfig);

jest.mock('../../app/StateManager');
const mockStateManager = require('../../app/StateManager');
// registerShutdownHandler and shutdown - defined inside a constructor
mockStateManager.registerShutdownHandler = jest.fn();
mockStateManager.shutdown = jest.fn();

jest.mock('../../app/kafka/KafkaConsumer');

jest.mock('../../app/redis/RedisManager');

const Application = require('../../app/App');

describe('Application Initialization', () => {
  it('Initialized Successfully', async () => {
    const app = new Application();
    app.kafkaConsumer.init = jest.fn(() => Promise.resolve());
    await app.init();

    expect(app.kafkaConsumer).toBeDefined();
    expect(app.redisManager).toBeDefined();
    expect(app.kafkaConsumer.init).toBeCalled();
    expect(app.kafkaConsumer.registerCallback).toBeCalled();
  });

  it('Initialized Unsuccessfully', async () => {
    const app = new Application();
    app.kafkaConsumer.init = jest.fn(() => Promise.reject());
    await app.init();

    expect(app.kafkaConsumer).toBeDefined();
    expect(app.redisManager).toBeDefined();
    expect(app.kafkaConsumer.init).toBeCalled();
    expect(app.kafkaConsumer.registerCallback).not.toBeCalled();
    expect(mockStateManager.shutdown).toBeCalled();
  });
});

describe('Processing Data', () => {
  let app;
  beforeEach(() => {
    app = new Application();
    app.redisManager.setAsync = jest.fn(() => Promise.resolve());
    app.redisManager.delAsync = jest.fn(() => Promise.resolve());
  });

  it('Ownership Create - Device', async () => {
    const msg = {
      metadata: { tenant: 'admin' },
      data: {
        eventType: 'ownership.create',
        eventData: {
          fingerprint: 'fingerprintX',
          belongsTo: {
            device: 'deviceX',
          },
        },
      },
    };

    const ack = jest.fn();
    await app.processData({ value: JSON.stringify(msg) }, ack);

    expect(app.redisManager.setAsync).toBeCalledWith(
      msg.data.eventData.fingerprint,
      `${msg.metadata.tenant}:${msg.data.eventData.belongsTo.device}`,
    );
    expect(ack).toBeCalled();
  });

  it('Ownership Update - Device', async () => {
    const msg = {
      metadata: { tenant: 'admin' },
      data: {
        eventType: 'ownership.update',
        eventData: {
          fingerprint: 'fingerprintX',
          belongsTo: {
            device: 'deviceY',
          },
        },
      },
    };

    const ack = jest.fn();
    await app.processData({ value: JSON.stringify(msg) }, ack);

    expect(app.redisManager.setAsync).toBeCalledWith(
      msg.data.eventData.fingerprint,
      `${msg.metadata.tenant}:${msg.data.eventData.belongsTo.device}`,
    );
    expect(ack).toBeCalled();
  });

  it('Ownership Delete - Device', async () => {
    const msg = {
      metadata: { tenant: 'admin' },
      data: {
        eventType: 'ownership.create',
        eventData: {
          fingerprint: 'fingerprintX',
          belongsTo: {
            device: 'deviceX',
          },
        },
      },
    };

    const ack = jest.fn();
    await app.processData({ value: JSON.stringify(msg) }, ack);

    expect(app.redisManager.setAsync).toBeCalledWith(
      msg.data.eventData.fingerprint,
      `${msg.metadata.tenant}:${msg.data.eventData.belongsTo.device}`,
    );
    expect(ack).toBeCalled();
  });

  it('Ownership Update - Device', async () => {
    const msg = {
      metadata: { tenant: 'admin' },
      data: {
        eventType: 'ownership.delete',
        eventData: {
          fingerprint: 'fingerprintX',
        },
      },
    };

    const ack = jest.fn();
    await app.processData({ value: JSON.stringify(msg) }, ack);

    expect(app.redisManager.delAsync).toBeCalledWith(
      msg.data.eventData.fingerprint,
    );
    expect(ack).toBeCalled();
  });

  it('Ownership Create - Application', async () => {
    const msg = {
      data: {
        eventType: 'ownership.create',
        eventData: {
          fingerprint: 'fingerprintX',
          belongsTo: {
            application: 'applicationX',
          },
        },
      },
    };

    const ack = jest.fn();
    await app.processData({ value: JSON.stringify(msg) }, ack);

    expect(app.redisManager.setAsync).toBeCalledWith(
      msg.data.eventData.fingerprint,
      `${msg.data.eventData.belongsTo.application}`,
    );
    expect(ack).toBeCalled();
  });

  it('Ownership Update - Application', async () => {
    const msg = {
      data: {
        eventType: 'ownership.update',
        eventData: {
          fingerprint: 'fingerprintX',
          belongsTo: {
            application: 'applicationY',
          },
        },
      },
    };

    const ack = jest.fn();
    await app.processData({ value: JSON.stringify(msg) }, ack);

    expect(app.redisManager.setAsync).toBeCalledWith(
      msg.data.eventData.fingerprint,
      `${msg.data.eventData.belongsTo.application}`,
    );
    expect(ack).toBeCalled();
  });

  it('Ownership Delete - Application', async () => {
    const msg = {
      data: {
        eventType: 'ownership.delete',
        eventData: {
          fingerprint: 'fingerprintX',
        },
      },
    };

    const ack = jest.fn();
    await app.processData({ value: JSON.stringify(msg) }, ack);

    expect(app.redisManager.delAsync).toBeCalledWith(
      msg.data.eventData.fingerprint,
    );
    expect(ack).toBeCalled();
  });

  it('Unexpected Event', async () => {
    const msg = {
      data: {
        eventType: 'unexpected.type',
        eventData: {
          fingerprint: 'fingerprintX',
        },
      },
    };

    const ack = jest.fn();
    await app.processData({ value: JSON.stringify(msg) }, ack);

    expect(app.redisManager.setAsync).not.toBeCalled();
    expect(app.redisManager.delAsync).not.toBeCalled();
    expect(ack).toBeCalled();
  });

  it('Failed to write to Redis', async () => {
    app.redisManager.setAsync = jest.fn(() => Promise.reject());
    const msg = {
      metadata: { tenant: 'admin' },
      data: {
        eventType: 'ownership.create',
        eventData: {
          fingerprint: 'fingerprintX',
          belongsTo: {
            device: 'deviceX',
          },
        },
      },
    };

    const ack = jest.fn();
    await app.processData({ value: JSON.stringify(msg) }, ack);

    expect(app.redisManager.setAsync).toBeCalled();
    expect(ack).not.toBeCalled();
    // called async, would be necessary an nack
    // expect(mockStateManager.shutdown).toBeCalled();
  });

  it('Missing data - create', async () => {
    app.redisManager.setAsync = jest.fn(() => Promise.reject());
    const msg = {
      metadata: {},
      data: {
        eventType: 'ownership.create',
        eventData: {
          fingerprint: 'fingerprintX',
          belongsTo: {
            device: 'deviceX',
          },
        },
      },
    };

    const ack = jest.fn();
    await app.processData({ value: JSON.stringify(msg) }, ack);

    expect(app.redisManager.setAsync).not.toBeCalled();
    expect(ack).not.toBeCalled();
    expect(mockStateManager.shutdown).toBeCalled();
  });

  it('Missing data - delete', async () => {
    app.redisManager.setAsync = jest.fn(() => Promise.reject());
    const msg = {
      metadata: { tenant: 'admin' },
      data: {
        eventType: 'ownership.delete',
      },
    };

    const ack = jest.fn();
    await app.processData({ value: JSON.stringify(msg) }, ack);

    expect(app.redisManager.delAsync).not.toBeCalled();
    expect(ack).not.toBeCalled();
    expect(mockStateManager.shutdown).toBeCalled();
  });

  it('Event is not a JSON', async () => {
    app.redisManager.setAsync = jest.fn(() => Promise.reject());
    const msg = '';

    const ack = jest.fn();
    await app.processData({ value: msg }, ack);

    expect(app.redisManager.delAsync).not.toBeCalled();
    expect(ack).not.toBeCalled();
    expect(mockStateManager.shutdown).toBeCalled();
  });
});
