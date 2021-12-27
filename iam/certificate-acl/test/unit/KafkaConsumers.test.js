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
};

jest.mock('@dojot/microservice-sdk');

const sdkMock = require('@dojot/microservice-sdk');

sdkMock.ConfigManager.getConfig = jest.fn(() => mockConfig);

const { ServiceStateManager } = require('@dojot/microservice-sdk');

const stateManagerMock = new ServiceStateManager();
// registerShutdownHandler - defined inside a constructor
stateManagerMock.registerShutdownHandler = jest.fn();

const KafkaConsumer = require('../../app/kafka/KafkaConsumer');

describe('KafkaConsumer Initialization', () => {
  it('Constructor', () => {
    const kconsumer = new KafkaConsumer(stateManagerMock);

    expect(kconsumer.suspended).toBeFalsy();
    expect(kconsumer.healthy).toBeFalsy();
    expect(kconsumer.consumer).toBeDefined();
    expect(kconsumer.registeredCallbacks).toBeDefined();
    expect(stateManagerMock.addHealthChecker).toBeCalled();
    expect(stateManagerMock.registerShutdownHandler).toBeCalled();
  });

  it('Init', () => {
    const kconsumer = new KafkaConsumer(stateManagerMock);
    kconsumer.init();
    expect(kconsumer.consumer.init).toBeCalled();
  });
});

describe('Register Callbacks', () => {
  it('Register when Kafka Consumer is suspended', () => {
    const kconsumer = new KafkaConsumer(stateManagerMock);
    kconsumer.suspended = true;

    const cb = jest.fn();
    const topic = 'topic.test';
    kconsumer.registerCallback(topic, cb);

    expect(kconsumer.registeredCallbacks.size).toBe(1);
    expect(kconsumer.registeredCallbacks.get(topic)).toMatchObject({
      id: null,
      cb,
    });
  });

  it('Register when Kafka Consumer is not suspended', () => {
    const kconsumer = new KafkaConsumer(stateManagerMock);
    kconsumer.suspended = false;
    const cb = jest.fn();
    const topic = 'topic.test';
    const cbId = 'ckId.test';
    kconsumer.consumer.registerCallback = jest.fn().mockReturnValueOnce(cbId);

    kconsumer.registerCallback(topic, cb);

    expect(kconsumer.consumer.registerCallback)
      .toBeCalledWith(topic, cb);
    expect(kconsumer.registeredCallbacks.size).toBe(1);
    expect(kconsumer.registeredCallbacks.get(topic)).toMatchObject({
      id: cbId,
      cb,
    });
  });

  it('Register callback twice', () => {
    const kconsumer = new KafkaConsumer(stateManagerMock);
    kconsumer.suspended = false;
    const cb = jest.fn();
    const topic = 'topic.test';
    const cbId = 'ckId.test';
    kconsumer.consumer.registerCallback = jest.fn().mockReturnValueOnce(cbId);

    kconsumer.registerCallback(topic, cb);
    expect(() => kconsumer.registerCallback(topic, cb)).toThrow();

    expect(kconsumer.registeredCallbacks.size).toBe(1);
    expect(kconsumer.registeredCallbacks.get(topic)).toMatchObject({
      id: cbId,
      cb,
    });
  });
});

describe('Suspend and Resume Processing Callbacks', () => {
  let kconsumer = null;
  const cb = jest.fn();
  const topic = 'topic.test';
  const cbId = 'ckId.test';

  beforeEach(() => {
    kconsumer = new KafkaConsumer(stateManagerMock);
    kconsumer.consumer.registerCallback = jest.fn().mockReturnValue(cbId);
    kconsumer.suspend = false;
    kconsumer.registerCallback(topic, cb);
  });

  it('Suspend when Kafka Consumer is not suspended', () => {
    kconsumer.suspend();

    expect(kconsumer.consumer.unregisterCallback)
      .toHaveBeenCalledWith(cbId);
    expect(kconsumer.registeredCallbacks.size).toBe(1);
    expect(kconsumer.registeredCallbacks.get(topic)).toMatchObject({
      id: null,
      cb,
    });
    expect(kconsumer.suspended).toBeTruthy();
  });

  it('Suspend when Kafka Consumer is already suspended', () => {
    kconsumer.suspend();
    kconsumer.suspend();

    expect(kconsumer.consumer.unregisterCallback)
      .toHaveBeenCalledTimes(1);
    expect(kconsumer.registeredCallbacks.size).toBe(1);
    expect(kconsumer.registeredCallbacks.get(topic)).toMatchObject({
      id: null,
      cb,
    });
    expect(kconsumer.suspended).toBeTruthy();
  });

  it('Resume when Kafka Consumer is suspended', () => {
    kconsumer.suspend();
    kconsumer.resume();

    expect(kconsumer.consumer.unregisterCallback)
      .toHaveBeenCalledTimes(1);
    expect(kconsumer.consumer.registerCallback)
      .toHaveBeenCalledWith(topic, cb);
    expect(kconsumer.registeredCallbacks.size).toBe(1);
    expect(kconsumer.registeredCallbacks.get(topic)).toMatchObject({
      id: cbId,
      cb,
    });
    expect(kconsumer.suspended).toBeFalsy();
  });

  it('Resume when Kafka Consumer is not suspended', () => {
    kconsumer.resume();

    expect(kconsumer.consumer.unregisterCallback)
      .not.toHaveBeenCalled();
    expect(kconsumer.registeredCallbacks.size).toBe(1);
    expect(kconsumer.registeredCallbacks.get(topic)).toMatchObject({
      id: cbId,
      cb,
    });
    expect(kconsumer.suspended).toBeFalsy();
  });
});

describe('Health-Check', () => {
  it('Unhealthy to Healthy', async () => {
    const kconsumer = new KafkaConsumer(stateManagerMock);
    kconsumer.healthy = false;
    kconsumer.consumer.getStatus = jest.fn(() => Promise.resolve({ connected: true }));

    const signalReady = jest.fn();
    const signalNotReady = jest.fn();

    await kconsumer.checkHealth(signalReady, signalNotReady);
    expect(signalReady).toHaveBeenCalled();
    expect(signalNotReady).not.toHaveBeenCalled();
    expect(kconsumer.healthy).toBeTruthy();
  });

  it('Healthy to Unhealthy', async () => {
    const kconsumer = new KafkaConsumer(stateManagerMock);
    kconsumer.healthy = true;
    kconsumer.consumer.getStatus = jest.fn(() => Promise.resolve({ connected: false }));

    const signalReady = jest.fn();
    const signalNotReady = jest.fn();

    await kconsumer.checkHealth(signalReady, signalNotReady);
    expect(signalReady).not.toHaveBeenCalled();
    expect(signalNotReady).toHaveBeenCalled();
    expect(kconsumer.healthy).toBeFalsy();
  });

  it('Continues Healthy', async () => {
    const kconsumer = new KafkaConsumer(stateManagerMock);
    kconsumer.healthy = true;
    kconsumer.consumer.getStatus = jest.fn(() => Promise.resolve({ connected: true }));

    const signalReady = jest.fn();
    const signalNotReady = jest.fn();

    await kconsumer.checkHealth(signalReady, signalNotReady);
    expect(signalReady).not.toHaveBeenCalled();
    expect(signalNotReady).not.toHaveBeenCalled();
    expect(kconsumer.healthy).toBeTruthy();
  });

  it('Continues Unhealthy', async () => {
    const kconsumer = new KafkaConsumer(stateManagerMock);
    kconsumer.healthy = false;
    kconsumer.consumer.getStatus = jest.fn(() => Promise.resolve({ connected: false }));

    const signalReady = jest.fn();
    const signalNotReady = jest.fn();

    await kconsumer.checkHealth(signalReady, signalNotReady);
    expect(signalReady).not.toHaveBeenCalled();
    expect(signalNotReady).not.toHaveBeenCalled();
    expect(kconsumer.healthy).toBeFalsy();
  });

  it('Failed to Get Status', async () => {
    const kconsumer = new KafkaConsumer(stateManagerMock);
    kconsumer.healthy = true;
    kconsumer.consumer.getStatus = jest.fn(() => Promise.reject(new Error('Error test message')));

    const signalReady = jest.fn();
    const signalNotReady = jest.fn();

    await kconsumer.checkHealth(signalReady, signalNotReady);
    expect(signalReady).not.toHaveBeenCalled();
    expect(signalNotReady).toHaveBeenCalled();
    expect(kconsumer.healthy).toBeFalsy();
  });
});

describe('Graceful Shutdown', () => {
  it('Finish Kafka Consumer', async () => {
    const kconsumer = new KafkaConsumer(stateManagerMock);
    kconsumer.consumer.finish = jest.fn(() => Promise.resolve());

    await kconsumer.shutdown();

    expect(kconsumer.consumer.finish).toBeCalled();
  });
});
