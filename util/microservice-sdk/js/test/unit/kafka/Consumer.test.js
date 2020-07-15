const Consumer = require('kafka/Consumer.js');

jest.mock('node-rdkafka');
jest.mock('logging/Logger.js');
jest.mock('kafka/CommitManager.js');
jest.mock('async');
jest.mock('uuid/v4');

const uuidMock = require('uuid/v4');

const CommitManagerMock = require('kafka/CommitManager.js');

const AsyncMock = require('async');

AsyncMock.queue = jest.fn((queueHandle) => {
  AsyncMock.queueHandle = queueHandle;
  AsyncMock.processingQueue = [];
  return {
    push: jest.fn((data, callback) => {
      AsyncMock.processingQueue = AsyncMock.processingQueue || [];
      AsyncMock.processingQueue.push({ data, callback });
    }),
    drain: jest.fn((drainCallback) => {
      AsyncMock.drainCallback = drainCallback;
    }),
    remove: jest.fn(() => {
    }),
    length: jest.fn(() => AsyncMock.processingQueue.length),
  };
});
AsyncMock.process = jest.fn(async () => {
  const element = AsyncMock.processingQueue.shift();
  await AsyncMock.queueHandle(element.data, element.callback);
  if (AsyncMock.processingQueue.length === 0) {
    await AsyncMock.drainCallback();
  }
});

const KafkaMock = require('node-rdkafka');

// Kafka Consumer Mock
KafkaMock.KafkaConsumer = class {
  constructor() {
    this.eventListener = {};
    this.emit = (event, data) => {
      this.eventListener[event](data);
    };
    this.on = jest.fn((event, cb) => {
      this.eventListener[event] = cb;
    });
    this.connect = jest.fn(() => {
      this.eventListener.ready();
    });
    this.commit = jest.fn();
    this.consume = jest.fn();
    this.subscribe = jest.fn();
    this.unsubscribe = jest.fn();
    this.pause = jest.fn();
    this.resume = jest.fn();
    this.assignments = jest.fn();
    this.unassign = jest.fn();
  }
};

expect.extend({
  toBeWithinRange(received, floor, ceiling) {
    const pass = received >= floor && received <= ceiling;
    if (pass) {
      return {
        message: () => `expected ${received} not to be within range ${floor} - ${ceiling}`,
        pass: true,
      };
    }

    return {
      message: () => `expected ${received} to be within range ${floor} - ${ceiling}`,
      pass: false,
    };
  },
});

test('Constructor: default', () => {
  const consumer = new Consumer();
  const expectedConfig = {
    'in.processing.max.messages': 1,
    'queued.max.messages.bytes': 10485760,
    'subscription.backoff.min.ms': 1000,
    'subscription.backoff.max.ms': 60000,
    'subscription.backoff.delta.ms': 1000,
    'commit.interval.ms': 5000,
    'kafka.consumer': {
      'enable.auto.commit': false,
    },
    'kafka.topic': {},
  };

  // check
  expect(consumer.config).toMatchObject(expectedConfig);
  expect(consumer.consumer).not.toBeNull();
  expect(consumer.commitManager).not.toBeNull();
  expect(consumer.msgQueue).not.toBeNull();
});

test('Constructor: divergent value for enable.auto.commit', () => {
  const consumer = new Consumer({ 'kafka.consumer': { 'enable.auto.commit': true } });
  const expectedConfig = {
    'in.processing.max.messages': 1,
    'queued.max.messages.bytes': 10485760,
    'subscription.backoff.min.ms': 1000,
    'subscription.backoff.max.ms': 60000,
    'subscription.backoff.delta.ms': 1000,
    'commit.interval.ms': 5000,
    'kafka.consumer': {
      'enable.auto.commit': false, // it must be disabled!
    },
    'kafka.topic': {},
  };

  // check
  expect(consumer.config).toMatchObject(expectedConfig);
  expect(consumer.consumer).not.toBeNull();
  expect(consumer.commitManager).not.toBeNull();
  expect(consumer.msgQueue).not.toBeNull();
});


test('Constructor: consumer and topic properties', () => {
  const consumer = new Consumer({
    'kafka.consumer': {
      'bootstrap.servers': ['kafka.server1', 'kafka.server2'],
    },
    'kafka.topic': {
      'auto.offset.reset': 'beginning',
    },
  });
  const expectedConfig = {
    'in.processing.max.messages': 1,
    'queued.max.messages.bytes': 10485760,
    'subscription.backoff.min.ms': 1000,
    'subscription.backoff.max.ms': 60000,
    'subscription.backoff.delta.ms': 1000,
    'commit.interval.ms': 5000,
    'kafka.consumer': {
      'bootstrap.servers': ['kafka.server1', 'kafka.server2'],
    },
    'kafka.topic': {
      'auto.offset.reset': 'beginning',
    },
  };

  // check
  expect(consumer.config).toMatchObject(expectedConfig);
  expect(consumer.consumer).not.toBeNull();
  expect(consumer.commitManager).not.toBeNull();
  expect(consumer.msgQueue).not.toBeNull();
});

test('Basic initialization', async () => {
  const consumer = new Consumer();
  consumer.refreshSubscriptions = jest.fn();
  await consumer.init();

  // check init behavior
  expect(consumer.isReady).toBe(true);
  expect(consumer.consumer.consume).toHaveBeenCalled();
  expect(consumer.refreshSubscriptions).toHaveBeenCalledTimes(1);
});

test('Failed initialization', async () => {
  expect.assertions(2);
  const consumer = new Consumer();
  consumer.consumer.connect = jest.fn((_unused, callback) => {
    callback(new Error('The kafka broker is not reachable.'));
  });

  try {
    await consumer.init();
  } catch (e) {
    expect(e).toEqual(new Error('The kafka broker is not reachable.'));
  }

  expect(consumer.isReady).toBe(false);
});

describe('Validates registerCallback', () => {
  it('using explicit topic', () => {
    const consumer = new Consumer({});
    consumer.refreshSubscriptions = jest.fn();

    const topicCallback = jest.fn();
    const targetTopic = 'amazingTopic';
    const expectedEntry = {
      id: 'random',
      callback: topicCallback,
    };
    uuidMock.mockReturnValueOnce(expectedEntry.id);

    consumer.registerCallback(targetTopic, topicCallback);

    // check registerCallback behavior
    expect(consumer.refreshSubscriptions).toHaveBeenCalledTimes(1);
    expect(Object.keys(consumer.topicMap)).toHaveLength(1);
    expect(consumer.topicMap).toHaveProperty(targetTopic);
    expect(consumer.topicMap[targetTopic]).toHaveLength(1);
    expect(consumer.topicMap[targetTopic]).toEqual(expect.arrayContaining([expectedEntry]));
  });

  it('using RegExp topic', () => {
    const consumer = new Consumer({});
    consumer.refreshSubscriptions = jest.fn();

    const topicCallback = jest.fn();
    const targetTopic = /^tenant.*/;
    const expectedEntry = {
      id: 'random',
      callback: topicCallback,
      regExp: targetTopic,
    };
    uuidMock.mockReturnValueOnce(expectedEntry.id);

    consumer.registerCallback(targetTopic, topicCallback);

    // check registerCallback behavior
    expect(consumer.refreshSubscriptions).toHaveBeenCalledTimes(1);
    expect(Object.keys(consumer.topicRegExpArray)).toHaveLength(1);
    expect(consumer.topicRegExpArray).toEqual(expect.arrayContaining([expectedEntry]));
  });

  it('registries on a repeated topic', () => {
    const consumer = new Consumer({});
    consumer.refreshSubscriptions = jest.fn();

    const targetTopic = 'amazingTopic';
    const mockedEntry = { id: 'entry', callback: jest.fn() };
    consumer.topicMap[targetTopic] = [mockedEntry];

    const topicCallback = jest.fn();
    const expectedEntry = {
      id: 'random',
      callback: topicCallback,
    };
    uuidMock.mockReturnValueOnce(expectedEntry.id);

    consumer.registerCallback(targetTopic, topicCallback);

    // check registerCallback behavior
    expect(consumer.refreshSubscriptions).not.toHaveBeenCalled();
    expect(Object.keys(consumer.topicMap)).toHaveLength(1);
    expect(consumer.topicMap).toHaveProperty(targetTopic);
    expect(consumer.topicMap[targetTopic]).toHaveLength(2);
    expect(consumer.topicMap[targetTopic]).toEqual(
      expect.arrayContaining([mockedEntry, expectedEntry]),
    );
  });
});

describe('Validates unregisterCallback', () => {
  let consumer = null;
  const entryRegExp1 = { id: 'entry1', callback: jest.fn(), regExp: /^tenant\/.*/ };
  const entryRegExp2 = { id: 'entry2', callback: jest.fn(), regExp: /^user\/.*/ };
  const entryExp1Elem1 = { id: 'entry3', callback: jest.fn() };
  const entryExp1Elem2 = { id: 'entry4', callback: jest.fn() };
  const entryExp2Elem1 = { id: 'entry5', callback: jest.fn() };
  const entryExp1 = 'user.juri';
  const entryExp2 = 'user.ryu';

  beforeEach(() => {
    consumer = new Consumer({});
    consumer.refreshSubscriptions = jest.fn();
    consumer.topicRegExpArray = [entryRegExp1, entryRegExp2];
    consumer.topicMap[entryExp1] = [entryExp1Elem1, entryExp1Elem2];
    consumer.topicMap[entryExp2] = [entryExp2Elem1];
  });
  test('explicit topic with one entry', async () => {
    consumer.unregisterCallback('entry5');

    expect(consumer.refreshSubscriptions).toHaveBeenCalledTimes(1);
    expect(Object.keys(consumer.topicMap)).toHaveLength(1);
    expect(consumer.topicMap).not.toHaveProperty(entryExp2);
    expect(consumer.topicMap[entryExp1]).toHaveLength(2);
    expect(consumer.topicRegExpArray).toHaveLength(2);
  });

  test('explicit topic with multiple entries', async () => {
    consumer.unregisterCallback('entry4');

    expect(consumer.refreshSubscriptions).not.toHaveBeenCalled();
    expect(Object.keys(consumer.topicMap)).toHaveLength(2);
    expect(consumer.topicMap[entryExp1]).toHaveLength(1);
    expect(consumer.topicRegExpArray).toHaveLength(2);
  });

  test('regExp topic', async () => {
    consumer.unregisterCallback('entry2');

    expect(consumer.refreshSubscriptions).toHaveBeenCalledTimes(1);
    expect(Object.keys(consumer.topicMap)).toHaveLength(2);
    expect(consumer.topicMap[entryExp1]).toHaveLength(2);
    expect(consumer.topicMap[entryExp2]).toHaveLength(1);
    expect(consumer.topicRegExpArray).toHaveLength(1);
    expect(consumer.topicRegExpArray).toEqual(expect.arrayContaining([entryRegExp1]));
  });
});

describe('Message processing', () => {
  test('receives a new message', () => {
    const consumer = new Consumer({});
    consumer.commitManager = new CommitManagerMock();
    consumer.msgQueue = AsyncMock.queue(jest.fn());
    consumer.consumer = new KafkaMock.KafkaConsumer();

    const publishedData = {
      value: Buffer.from('konnichiwa'), // message contents as a Buffer
      size: 10, // size of the message, in bytes
      topic: 'tenant/dojot', // topic the message comes from
      offset: 1337, // offset the message was read from
      partition: 1, // partition the message was on
      key: 'someKey', // key of the message if present
      timestamp: 1510325354780, // timestamp of message creation
    };
    consumer.onData(publishedData);

    expect(consumer.currQueueBytes).toBe(publishedData.size);
    expect(consumer.msgQueue.length()).toBe(1);
    expect(consumer.consumer.pause).toHaveBeenCalledTimes(0);
    expect(consumer.pause).toBeFalsy();
  });

  test('receives a new message (queue overflow)', () => {
    const consumer = new Consumer({});
    consumer.commitManager = new CommitManagerMock();
    consumer.msgQueue = AsyncMock.queue(jest.fn());
    consumer.consumer = new KafkaMock.KafkaConsumer();

    const publishedData = {
      value: Buffer.from('konnichiwa'), // message contents as a Buffer
      size: 2e10, // size of the message, in bytes
      topic: 'tenant/dojot', // topic the message comes from
      offset: 1337, // offset the message was read from
      partition: 1, // partition the message was on
      key: 'someKey', // key of the message if present
      timestamp: 1510325354780, // timestamp of message creation
    };
    consumer.onData(publishedData);

    expect(consumer.currQueueBytes).toBe(publishedData.size);
    expect(consumer.msgQueue.length()).toBe(1);
    expect(consumer.consumer.pause).toHaveBeenCalledTimes(1);
    expect(consumer.isPaused).toBeTruthy();
  });
});

describe('handle kafka', () => {
  let consumer = null;
  beforeEach(() => {
    consumer = new Consumer({});
    consumer.commitManager = new CommitManagerMock();
    consumer.topicRegExpArray = [
      { id: 'entry1', callback: jest.fn(), regExp: /^tenant\/.*/ },
      { id: 'entry2', callback: jest.fn(), regExp: /^user\/.*/ },
      { id: 'entry3', callback: jest.fn(() => { throw new Error('Mocked error'); }), regExp: /^troublemaker\/.*/ },
    ];
    consumer.topicMap['user/juri'] = [
      { id: 'entry4', callback: jest.fn() },
      { id: 'entry5', callback: jest.fn() },
    ];
    consumer.topicMap['troublemaker/denis'] = [
      { id: 'entry4', callback: jest.fn() },
      { id: 'entry5', callback: jest.fn(() => { throw new Error('Mocked error'); }) },
    ];
  });
  test('Message processing with RegEx topic', async () => {
    const publishedData = {
      value: Buffer.from('konnichiwa'), // message contents as a Buffer
      size: 10, // size of the message, in bytes
      topic: 'tenant/dojot', // topic the message comes from
      offset: 1337, // offset the message was read from
      partition: 1, // partition the message was on
      key: 'someKey', // key of the message if present
      timestamp: 1510325354780, // timestamp of message creation
    };
    consumer.invokeInterestedCallbacks(publishedData);

    expect(consumer.topicRegExpArray[0].callback).toHaveBeenCalledWith(publishedData);
    expect(consumer.topicRegExpArray[0].callback).toHaveBeenCalledTimes(1);

    expect(consumer.commitManager.notifyFinishedProcessing).toHaveBeenCalledTimes(1);
  });

  test('Message processing with RegEx and explicit topic', async () => {
    const publishedData = {
      value: Buffer.from('konnichiwa'), // message contents as a Buffer
      size: 10, // size of the message, in bytes
      topic: 'user/juri', // topic the message comes from
      offset: 1337, // offset the message was read from
      partition: 1, // partition the message was on
      key: 'someKey', // key of the message if present
      timestamp: 1510325354780, // timestamp of message creation
    };
    consumer.invokeInterestedCallbacks(publishedData);

    expect(consumer.topicRegExpArray[1].callback).toHaveBeenCalledWith(publishedData);
    expect(consumer.topicRegExpArray[1].callback).toHaveBeenCalledTimes(1);
    expect(consumer.topicMap[publishedData.topic][0].callback).toHaveBeenCalledWith(publishedData);
    expect(consumer.topicMap[publishedData.topic][0].callback).toHaveBeenCalledTimes(1);
    expect(consumer.topicMap[publishedData.topic][1].callback).toHaveBeenCalledWith(publishedData);
    expect(consumer.topicMap[publishedData.topic][1].callback).toHaveBeenCalledTimes(1);

    expect(consumer.commitManager.notifyFinishedProcessing).toHaveBeenCalledTimes(1);
  });

  test('Message processing with failure', async () => {
    const publishedData = {
      value: Buffer.from('konnichiwa'), // message contents as a Buffer
      size: 10, // size of the message, in bytes
      topic: 'troublemaker/denis', // topic the message comes from
      offset: 1337, // offset the message was read from
      partition: 1, // partition the message was on
      key: 'someKey', // key of the message if present
      timestamp: 1510325354780, // timestamp of message creation
    };
    consumer.invokeInterestedCallbacks(publishedData);

    expect(consumer.topicRegExpArray[2].callback).toHaveBeenCalledWith(publishedData);
    expect(consumer.topicRegExpArray[2].callback).toHaveBeenCalledTimes(1);
    expect(consumer.topicMap[publishedData.topic][0].callback).toHaveBeenCalledWith(publishedData);
    expect(consumer.topicMap[publishedData.topic][0].callback).toHaveBeenCalledTimes(1);
    expect(consumer.topicMap[publishedData.topic][1].callback).toHaveBeenCalledWith(publishedData);
    expect(consumer.topicMap[publishedData.topic][1].callback).toHaveBeenCalledTimes(1);

    expect(consumer.commitManager.notifyFinishedProcessing).toHaveBeenCalledTimes(1);
  });
});

describe('Refresh subscription', () => {
  test('Refresh subscription', () => {
    // sets the fake timer to analyze the refreshSubscriptions
    jest.useFakeTimers();

    // test subject
    const consumer = new Consumer({});

    consumer.consumer = new KafkaMock.KafkaConsumer();
    consumer.isReady = true;
    const topic = 'tenant/dojot';
    consumer.topicMap[topic] = [
      { id: 'entry1', callback: jest.fn() },
    ];
    const regExpTopic = /^tenant\/.*/;
    consumer.topicRegExpArray = [
      { id: 'entry2', callback: jest.fn(), regExp: regExpTopic },
    ];
    consumer.refreshSubscriptions();

    // execute the setTimeout callbacks
    jest.runOnlyPendingTimers();

    expect(consumer.consumer.unsubscribe).toHaveBeenCalledTimes(1);
    expect(consumer.consumer.subscribe).toHaveBeenCalledTimes(1);
    expect(consumer.consumer.subscribe).toHaveBeenCalledWith([topic, regExpTopic]);
  });

  test('Refresh subscription with retry', (done) => {
    // sets the fake timer to analyze the refreshSubscriptions
    jest.useFakeTimers();

    const consumer = new Consumer(
      {
        'subscription.backoff.min.ms': 1000,
        'subscription.backoff.max.ms': 10000,
        'subscription.backoff.delta.ms': 1000,
      },
    );

    consumer.consumer = new KafkaMock.KafkaConsumer();
    consumer.isReady = true;
    const topic = 'tenant/dojot';
    consumer.topicMap[topic] = [
      { id: 'entry', callback: jest.fn() },
    ];

    consumer.consumer.subscribe = jest.fn();

    // Expected calls:
    // (1) subscriptionProcedure(/*retries*/ 0) -> fails
    // (2) t = 1000 + random(0, 1000): subscriptionProcedure(/*retries*/ 1) -> fails
    // (3) t = 2000 + random(0, 1000): subscriptionProcedure(/*retries*/ 2) -> fails
    // (4) t = 4000 + random(0, 1000): subscriptionProcedure(/*retries*/ 3) -> fails
    // (5) t = 8000 + random(0, 1000): subscriptionProcedure(/*retries*/ 4) -> fails
    // (6) t = 10000: subscriptionProcedure(/*retries*/ 4) -> fails
    // (7) t = 10000: succeeded
    consumer.consumer.subscribe
      .mockImplementationOnce(() => {
        throw Error('subscription mocked error');
      })
      .mockImplementationOnce(() => {
        throw Error('subscription mocked error');
      })
      .mockImplementationOnce(() => {
        throw Error('subscription mocked error');
      })
      .mockImplementationOnce(() => {
        throw Error('subscription mocked error');
      })
      .mockImplementationOnce(() => {
        throw Error('subscription mocked error');
      })
      .mockImplementationOnce(() => {
        throw Error('subscription mocked error');
      })
      .mockImplementationOnce(() => {
        done();
      });

    consumer.refreshSubscriptions();

    // check registerCallback behavior
    expect(consumer.isReady).toBe(true);
    expect(Object.keys(consumer.topicMap)).toHaveLength(1);

    jest.runAllTimers();

    expect(setTimeout).toHaveBeenCalledTimes(6);
    expect(setTimeout).toHaveBeenCalledWith(
      expect.any(Function), expect.toBeWithinRange(1000, 2000), 1,
    );
    expect(setTimeout).toHaveBeenCalledWith(
      expect.any(Function), expect.toBeWithinRange(2000, 3000), 2,
    );
    expect(setTimeout).toHaveBeenCalledWith(
      expect.any(Function), expect.toBeWithinRange(4000, 5000), 3,
    );
    expect(setTimeout).toHaveBeenCalledWith(
      expect.any(Function), expect.toBeWithinRange(8000, 9000), 4,
    );
    expect(setTimeout).toHaveBeenCalledWith(expect.any(Function), 10000, 5);
    expect(setTimeout).toHaveBeenCalledWith(expect.any(Function), 10000, 6);

    expect(consumer.consumer.unsubscribe).toHaveBeenCalledTimes(7);
    expect(consumer.consumer.subscribe).toHaveBeenCalledTimes(7);
    expect(consumer.consumer.subscribe).toHaveBeenCalledWith([topic]);
  });
});

describe('Resume consumer', () => {
  it('when paused', () => {
    const consumer = new Consumer({});
    consumer.isPaused = true;
    consumer.consumer = new KafkaMock.KafkaConsumer();

    consumer.resumeConsumer();

    expect(consumer.consumer.resume).toHaveBeenCalledTimes(1);
    expect(consumer.isPaused).toBeFalsy();
  });

  it('when is not paused', () => {
    const consumer = new Consumer({});
    consumer.consumer = new KafkaMock.KafkaConsumer();

    consumer.resumeConsumer();

    expect(consumer.consumer.resume).toHaveBeenCalledTimes(0);
    expect(consumer.isPaused).toBeFalsy();
  });
});

test('Rebalance - revoke partitions', () => {
  const consumer = new Consumer({});
  consumer.consumer = new KafkaMock.KafkaConsumer();
  consumer.commitManager = new CommitManagerMock();
  consumer.msgQueue = AsyncMock.queue(jest.fn());

  consumer.onRebalance({ code: KafkaMock.CODES.ERRORS.ERR__REVOKE_PARTITIONS }, {});

  expect(consumer.consumer.unassign).toHaveBeenCalledTimes(1);
  expect(consumer.commitManager.onRebalance).toHaveBeenCalledTimes(1);
  expect(consumer.msgQueue.remove).toHaveBeenCalledTimes(1);
  expect(consumer.consumer.resume).toHaveBeenCalledTimes(0);
  expect(consumer.isPaused).toBeFalsy();
});

test('Rebalance - revoke partitions: paused case', () => {
  const consumer = new Consumer({});
  consumer.consumer = new KafkaMock.KafkaConsumer();
  consumer.commitManager = new CommitManagerMock();
  consumer.msgQueue = AsyncMock.queue(jest.fn());
  consumer.isPaused = true;

  consumer.onRebalance({ code: KafkaMock.CODES.ERRORS.ERR__REVOKE_PARTITIONS }, {});

  expect(consumer.consumer.unassign).toHaveBeenCalledTimes(1);
  expect(consumer.commitManager.onRebalance).toHaveBeenCalledTimes(1);
  expect(consumer.msgQueue.remove).toHaveBeenCalledTimes(1);
  expect(consumer.consumer.resume).toHaveBeenCalledTimes(1);
  expect(consumer.isPaused).toBeFalsy();
});

test('Rebalance - assign partitions', () => {
  const consumer = new Consumer({});
  consumer.consumer.assign = jest.fn();
  consumer.onRebalance({ code: KafkaMock.CODES.ERRORS.ERR__ASSIGN_PARTITIONS }, [1, 3, 5]);
  expect(consumer.consumer.assign).toHaveBeenCalledWith([1, 3, 5]);
});
