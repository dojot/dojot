const mockConfig = {
  consumer: {
    'group.id': 'kafka_test',
    'metadata.broker.list': 'kafka:9092',
  },
  topic: {
    'auto.offset.reset': 'largest',
  },
  healthcheck: {
    'kafka.interval.ms': 10,
  },
};

const mockMicroServiceSdk = {
  ConfigManager: {
    getConfig: jest.fn(() => mockConfig),
    transformObjectKeys: jest.fn((obj) => obj),
  },
  Kafka: {
    Consumer: jest.fn(() => ({
      getStatus: jest.fn(() => Promise.resolve()),
      finish: jest.fn(() => Promise.resolve()),
    })),
    Producer: jest.fn(),
  },
  ServiceStateManager: jest.fn(() => ({
    registerService: jest.fn(),
    signalReady: jest.fn(),
    signalNotReady: jest.fn(),
    addHealthChecker: jest.fn((service, callback) => callback()),
    registerShutdownHandler: jest.fn(),
  })),
  Logger: jest.fn(() => ({
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
  })),
};

jest.mock('@dojot/microservice-sdk', () => mockMicroServiceSdk);
jest.mock('redis');

const { Kafka: { Consumer } } = require('@dojot/microservice-sdk');
const KafkaWSConsumers = require('../../app/kafka/KafkaConsumer');

let kafkaWSConsumers = null;
describe('Testing KafkaWSConsumers - works fine', () => {
  beforeAll(() => {
    Consumer.mockReturnValue({
      init: jest.fn()
        .mockImplementationOnce(() => Promise.resolve()),
      registerCallback: jest.fn()
        .mockReturnValueOnce('idCallback1'),
      getStatus: jest.fn()
        .mockImplementationOnce(() => Promise.resolve({ connected: true })),
      finish: jest.fn()
        .mockImplementationOnce(() => Promise.resolve()),
    });
    kafkaWSConsumers = new KafkaWSConsumers();
  });
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('Should test health check function', () => {
    Consumer().getStatus
      .mockImplementationOnce(() => Promise.resolve({ connected: false }))
      .mockImplementationOnce(() => Promise.reject())
      .mockImplementationOnce(() => Promise.reject());

    const ready = jest.fn();
    const notReady = jest.fn();
    kafkaWSConsumers.checkHealth(ready, notReady);
    expect(mockMicroServiceSdk.Kafka.Consumer().getStatus).toHaveBeenCalledTimes(1);

    // else branch
    kafkaWSConsumers.checkHealth(ready, notReady);
    expect(mockMicroServiceSdk.Kafka.Consumer().getStatus).toHaveBeenCalledTimes(2);

    // reject status
    kafkaWSConsumers.checkHealth(ready, notReady);
    expect(mockMicroServiceSdk.Kafka.Consumer().getStatus).toHaveBeenCalledTimes(3);

    Consumer.mockClear();
  });

  it('should finish - graceful shutdown', () => {
    kafkaWSConsumers.shutdownProcess();
    expect(mockMicroServiceSdk.Kafka.Consumer().finish).toHaveBeenCalledTimes(1);
  });

  it('Should init correctly ', async () => {
    let someError = false;
    try {
      await kafkaWSConsumers.init();
    } catch (e) {
      someError = true;
    }
    expect(someError).toBe(false);
  });

  it('Should register a callback ', () => {
    kafkaWSConsumers.registerCallback('topic1', () => { });
    expect(kafkaWSConsumers.registeredCallbacks.get('topic1')).toStrictEqual('idCallback1');
  });

  it('Shouldnt register a callback already exist ', () => {
    let someError = false;
    try {
      kafkaWSConsumers.registerCallback('topic1', () => { });
    } catch (e) {
      someError = true;
    }
    expect(someError).toBe(true);
  });
});

describe('Should not init correctly', () => {
  beforeAll(() => {
    Consumer.mockReturnValue({
      init: jest.fn()
        .mockImplementationOnce(() => Promise.reject(new Error('Error'))),
      getStatus: jest.fn()
        .mockImplementationOnce(() => Promise.reject(new Error('Error'))),
    });
    kafkaWSConsumers = new KafkaWSConsumers();
  });
  beforeEach(() => {
    jest.clearAllMocks();
  });
});
