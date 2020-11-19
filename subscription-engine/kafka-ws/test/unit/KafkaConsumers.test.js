const mockConfig = {
  consumer: {
    'group.id': 'kafka_test',
    'metadata.broker.list': 'kafka:9092',
  },
  topic: {
    'auto.offset.reset': 'largest',
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
  })),
};

jest.mock('@dojot/microservice-sdk', () => mockMicroServiceSdk);

const { Kafka: { Consumer } } = require('@dojot/microservice-sdk');
const KafkaWSConsumers = require('../../app/Kafka/KafkaConsumer');

let kafkaWSConsumers = null;
describe('Testing KafkaWSConsumers - works fine', () => {
  beforeAll(() => {
    Consumer.mockReturnValue({
      init: jest.fn()
        .mockImplementationOnce(() => Promise.resolve()),
      registerCallback: jest.fn()
        .mockReturnValueOnce('idCallback1'),
      unregisterCallback: jest.fn()
        .mockImplementationOnce(() => () => Promise.resolve()),
      getStatus: jest.fn()
        .mockImplementationOnce(() => Promise.resolve({ connected: true })),
    });
    kafkaWSConsumers = new KafkaWSConsumers();
  });
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('Should test health check function', () => {
    Consumer().getStatus
      .mockImplementationOnce(() => Promise.resolve({ connected: true }))
      .mockImplementationOnce(() => Promise.resolve({ connected: false }))
      .mockImplementationOnce(() => Promise.reject());

    const ready = jest.fn();
    const notReady = jest.fn();
    kafkaWSConsumers.healthChecker(ready, notReady);
    expect(mockMicroServiceSdk.Kafka.Consumer().getStatus).toHaveBeenCalledTimes(1);

    // else branch
    kafkaWSConsumers.healthChecker(ready, notReady);
    expect(mockMicroServiceSdk.Kafka.Consumer().getStatus).toHaveBeenCalledTimes(2);

    // reject status
    kafkaWSConsumers.healthChecker(ready, notReady);
    expect(mockMicroServiceSdk.Kafka.Consumer().getStatus).toHaveBeenCalledTimes(3);

    Consumer.mockClear();
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

  it('Should unregister a callbacks ', () => {
    const idCallback = kafkaWSConsumers.registeredCallbacks.get('topic1');
    kafkaWSConsumers.unregisterCallback('topic1');
    expect(kafkaWSConsumers.consumer.unregisterCallback).toHaveBeenCalledWith(idCallback);
  });

  it('Shouldnt unregister a callbacks doesnt exist ', () => {
    let someError = false;
    try {
      kafkaWSConsumers.unregisterCallback('topic1');
    } catch (e) {
      someError = true;
    }
    expect(someError).toBe(true);
  });

  it('Should unregister all callbacks ', () => {
    kafkaWSConsumers.registerCallback('topic2', () => { });
    kafkaWSConsumers.registerCallback('topic3', () => { });

    const idCallback2 = kafkaWSConsumers.registeredCallbacks.get('topic2');
    const idCallback3 = kafkaWSConsumers.registeredCallbacks.get('topic3');

    kafkaWSConsumers.unregisterAllCallbacks();

    expect(kafkaWSConsumers.consumer.unregisterCallback).toHaveBeenCalledWith(idCallback2);
    expect(kafkaWSConsumers.consumer.unregisterCallback).toHaveBeenCalledWith(idCallback3);
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

  it('not init ', async () => {
    let someError = false;
    try {
      await kafkaWSConsumers.init();
    } catch (e) {
      someError = true;
    }
    expect(someError).toBe(true);
  });
});
