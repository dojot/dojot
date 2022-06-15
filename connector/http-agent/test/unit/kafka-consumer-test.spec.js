/* eslint-disable class-methods-use-this */
class MockConsumer {
  constructor(config, logger) {
    this.config = config;
    this.logger = logger;
  }

  init() {
  }

  registerCallback() {
  }

  getStatus() {
  }

  unregisterCallback() {

  }
}

jest.mock('@dojot/microservice-sdk', () => ({
  Kafka: {
    Consumer: MockConsumer,
  },
}));

const LoggerMock = {
  warn: jest.fn(),
  info: jest.fn(),
  debug: jest.fn(),
  error: jest.fn(),
};

const tenantServiceMock = {
  create: jest.fn(() => Promise.resolve('')),
  remove: jest.fn(() => Promise.resolve('')),
};

const config = {
  'enable.async.commit': true,
  'kafka.consumer': '',
  'kafka.topic': '',
  subscribe: {
    'topics.regex.tenants': '',
  },
};

const KafkaConsumer = require('../../app/kafka/ConsumerMessages');

describe('KafkaConsumer', () => {
  let kafkaConsumer;
  beforeEach(() => {
    kafkaConsumer = new KafkaConsumer(tenantServiceMock, config, LoggerMock);
  });

  it('Should init consumer', async () => {
    kafkaConsumer.initCallbackForNewTenantEvents = jest.fn();

    await kafkaConsumer.init();

    expect.anything();
  });

  it('Should register callback for new tenant events', async () => {
    kafkaConsumer.getCallbackForNewTenantEvents = jest.fn();

    await kafkaConsumer.initCallbackForNewTenantEvents();

    expect(kafkaConsumer.getCallbackForNewTenantEvents)
      .toHaveBeenCalled();
  });

  it('Should return a callback for new tenant events', async () => {
    const callback = await kafkaConsumer.getCallbackForNewTenantEvents();

    expect(callback).toBeDefined();
  });

  it('Should create a new tenant', async () => {
    const callback = await kafkaConsumer.getCallbackForNewTenantEvents();
    const data = {
      value: '{"type":"CREATE", "tenant": "test", "signatureKey": {}}',
    };
    const ack = jest.fn();
    callback(data, ack);

    expect(tenantServiceMock.create).toHaveBeenCalled();
  });

  it('Should remove a tenant', async () => {
    const callback = await kafkaConsumer.getCallbackForNewTenantEvents();
    const data = {
      value: '{"type":"DELETE"}',
    };
    const ack = jest.fn();

    callback(data, ack);

    expect(tenantServiceMock.remove).toHaveBeenCalled();
  });

  it('Should handle the error', async () => {
    const callback = await kafkaConsumer.getCallbackForNewTenantEvents();
    expect.assertions(1);

    const data = {
      value: '{"type":"DELETE"}',
    };
    const ack = jest.fn();
    tenantServiceMock.remove.mockRejectedValueOnce(new Error('Error'));
    LoggerMock.error.mockImplementationOnce((message) => {
      expect(message).toBeDefined();
    });

    callback(data, ack);
  });

  it('Should return true, when connected', async () => {
    kafkaConsumer.getStatus = jest.fn(() => ({
      connected: {},
    }));

    const connected = await kafkaConsumer.isConnected();
    expect(connected).toBeTruthy();
  });

  it('Should return false, when disconnected', async () => {
    kafkaConsumer.getStatus = jest.fn(() => ({
      connected: undefined,
    }));

    const connected = await kafkaConsumer.isConnected();
    expect(connected).toBeFalsy();
  });

  it('Should return false, when the connection cannot be verified', async () => {
    kafkaConsumer.getStatus = jest.fn(() => {
      throw Error('error');
    });

    const connected = await kafkaConsumer.isConnected();
    expect(connected).toBeFalsy();
  });

  it('Should unregister callbacks, when there are callbacks', async () => {
    kafkaConsumer.idCallbackTenant = 1;
    await kafkaConsumer.unregisterCallbacks();

    expect.anything();
  });

  it('should not throw any error, when there are not callbacks', async () => {
    await kafkaConsumer.unregisterCallbacks();

    expect.anything();
  });
});
