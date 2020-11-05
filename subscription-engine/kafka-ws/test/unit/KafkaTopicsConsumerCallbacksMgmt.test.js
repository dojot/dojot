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
    Consumer: jest.fn(),
    Producer: jest.fn(),
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

const KafkaTopicsCallbacksMgmt = require('../../app/Kafka/KafkaTopicsConsumerCallbacksMgmt');

jest.mock('../../app/Kafka/KafkaConsumer');

let kafkaTopicsCallbacksMgmt = null;
describe('Testing KafkaTopicsConsumerCallbacksMgmt - works fine', () => {
  beforeAll(() => {
    kafkaTopicsCallbacksMgmt = new KafkaTopicsCallbacksMgmt();
  });
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('Should init correctly ', async () => {
    kafkaTopicsCallbacksMgmt.kafka.init = jest.fn()
      .mockImplementationOnce(() => Promise.resolve());
    let someError = false;
    try {
      await kafkaTopicsCallbacksMgmt.init();
    } catch (e) {
      someError = true;
    }
    expect(someError).toBe(false);
  });

  it('addCallbackToTopic 2 callbacks to same topic and call createCallbackKafkaConsumer ', () => {
    kafkaTopicsCallbacksMgmt.kafka.registeredCallbacks = new Map();
    kafkaTopicsCallbacksMgmt.kafka.registerCallback = jest.fn();

    const callbackA = jest.fn();
    kafkaTopicsCallbacksMgmt.addCallback('topic1', '1', callbackA);

    kafkaTopicsCallbacksMgmt.kafka.registeredCallbacks.set('topic1', 'x1');

    const callbackB = jest.fn();
    kafkaTopicsCallbacksMgmt.addCallback('topic1', '2', callbackB);

    expect(kafkaTopicsCallbacksMgmt.kafka.registerCallback).toHaveBeenCalledTimes(1);

    const callback = kafkaTopicsCallbacksMgmt.createCallbackKafkaConsumer('topic1');

    callback({ value: '123' });

    expect(callbackA).toHaveBeenCalledTimes(1);
    expect(callbackB).toHaveBeenCalledTimes(1);
  });

  it('removeCallback 2 to same topic callbacks', () => {
    kafkaTopicsCallbacksMgmt.kafka.unregisterCallback = jest.fn();
    kafkaTopicsCallbacksMgmt.removeCallback('topic1', '1');
    kafkaTopicsCallbacksMgmt.removeCallback('topic1', '2');
    expect(kafkaTopicsCallbacksMgmt.kafka.unregisterCallback).toHaveBeenCalledTimes(1);
  });

  it('createCallbackKafkaConsumer to a not exist topic', () => {
    const callback = kafkaTopicsCallbacksMgmt.createCallbackKafkaConsumer('topic10');
    expect(callback).toBe(null);
  });
});

describe('Should not init correctly', () => {
  beforeAll(() => {
    kafkaTopicsCallbacksMgmt = new KafkaTopicsCallbacksMgmt();
  });
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('Should not init correctly ', async () => {
    kafkaTopicsCallbacksMgmt.kafka.init = jest.fn()
      .mockImplementationOnce(() => Promise.reject(new Error('Error')));
    let someError = false;
    try {
      await kafkaTopicsCallbacksMgmt.init();
    } catch (e) {
      someError = true;
    }
    expect(someError).toBe(true);
  });
});
