/* eslint-disable import/no-unresolved */
// eslint-disable-next-line import/extensions
const ConsumerBackPressure = require('kafka/ConsumerBackPressure.js');

jest.mock('node-rdkafka');
jest.mock('kafka/CommitManager.js');
jest.mock('async');
jest.mock('@dojot/dojot-module-logger');
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

test('Basic initialization', async () => {
  const consumer = new ConsumerBackPressure({});
  consumer.refreshSubscriptions = jest.fn();
  await consumer.init();

  // check init behavior
  expect(consumer.isReady).toBe(true);
  expect(consumer.consumer.consume).toHaveBeenCalled();
  expect(consumer.refreshSubscriptions).toHaveBeenCalledTimes(1);
});

describe('Validates registerCallback', () => {
  it('using explicit topic', () => {
    const consumer = new ConsumerBackPressure({});
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
    const consumer = new ConsumerBackPressure({});
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
    const consumer = new ConsumerBackPressure({});
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
    consumer = new ConsumerBackPressure({});
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
    const consumer = new ConsumerBackPressure({});
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
    const consumer = new ConsumerBackPressure({});
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
    consumer = new ConsumerBackPressure({});
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

describe('Validate setters', () => {
  test('setHoldOffTimeSubscription', async () => {
    const consumer = new ConsumerBackPressure({});

    const holdOffTimeSubscription = 100;
    consumer.setHoldOffTimeSubscription(holdOffTimeSubscription);
    expect(consumer.holdOffTimeSubscription).toBe(holdOffTimeSubscription);
  });

  test('setHoldOffFactorTimeSubscription', async () => {
    const consumer = new ConsumerBackPressure({});

    const holdOffFactorTimeSubscription = 100.0;
    consumer.setHoldOffFactorTimeSubscription(holdOffFactorTimeSubscription);
    expect(consumer.holdOffFactorTimeSubscription).toBeCloseTo(holdOffFactorTimeSubscription);
  });

  test('setMaxHoldOffTimeSubscription', async () => {
    const consumer = new ConsumerBackPressure({});

    const maxHoldOffTimeSubscription = 100;
    consumer.setMaxHoldOffTimeSubscription(maxHoldOffTimeSubscription);
    expect(consumer.maxHoldOffTimeSubscription).toBe(maxHoldOffTimeSubscription);
  });

  test('setMaxParallelHandlers', async () => {
    const consumer = new ConsumerBackPressure({});

    const maxParallelHandlers = 10;
    consumer.setMaxParallelHandlers(maxParallelHandlers);
    expect(consumer.maxParallelHandlers).toBe(maxParallelHandlers);
  });

  test('setMaxQueueBytes', async () => {
    const consumer = new ConsumerBackPressure({});

    const maxQueueBytes = 30000;
    consumer.setMaxQueueBytes(maxQueueBytes);
    expect(consumer.maxQueueBytes).toBe(maxQueueBytes);
  });
});

describe('Refresh subscription', () => {
  test('Refresh subscription', async () => {
    // sets the fake timer to analyze the refreshSubscriptions
    jest.useFakeTimers();

    // test subject
    const consumer = new ConsumerBackPressure({});

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
    const holdOffTime = 1000;
    consumer.refreshSubscriptions(holdOffTime);

    // execute the setTimeout callbacks
    jest.runOnlyPendingTimers();

    expect(consumer.consumer.unsubscribe).toHaveBeenCalledTimes(1);
    expect(consumer.consumer.subscribe).toHaveBeenCalledTimes(1);
    expect(consumer.consumer.subscribe).toHaveBeenCalledWith([topic, regExpTopic]);
  });

  test('Refresh subscription with retry', async (done) => {
    // sets the fake timer to analyze the refreshSubscriptions
    jest.useFakeTimers();

    // test subject
    const consumer = new ConsumerBackPressure({});

    consumer.holdOffTimeSubscription = 1000;
    consumer.holdOffFactorTimeSubscription = 2.0;
    consumer.maxHoldOffTimeSubscription = 3000;

    consumer.consumer = new KafkaMock.KafkaConsumer();
    consumer.isReady = true;
    const topic = 'tenant/dojot';
    consumer.topicMap[topic] = [
      { id: 'entry', callback: jest.fn() },
    ];
    consumer.refreshSubscriptions(0);

    // check registerCallback behavior
    expect(consumer.isReady).toBe(true);
    expect(Object.keys(consumer.topicMap)).toHaveLength(1);

    consumer.consumer.subscribe = jest.fn(() => {});

    consumer.consumer.subscribe.mockImplementationOnce(() => {
      throw Error('subscription mocked error');
    }).mockImplementationOnce(() => {
      throw Error('subscription mocked error');
    }).mockImplementationOnce(() => {
      throw Error('subscription mocked error');
    }).mockImplementationOnce(() => {
      done();
    });

    jest.advanceTimersByTime(10000);

    expect(setTimeout).toHaveBeenCalledTimes(4);
    expect(setTimeout).toHaveBeenCalledWith(expect.any(Function), 0, 0);
    expect(setTimeout).toHaveBeenCalledWith(expect.any(Function), 1000, 1000);
    expect(setTimeout).toHaveBeenCalledWith(expect.any(Function), 2000, 2000);
    expect(setTimeout).toHaveBeenCalledWith(expect.any(Function), 3000, 3000);

    expect(consumer.consumer.unsubscribe).toHaveBeenCalledTimes(4);
    expect(consumer.consumer.subscribe).toHaveBeenCalledTimes(4);
    expect(consumer.consumer.subscribe).toHaveBeenCalledWith([topic]);
  });
});

describe('resume consumer', () => {
  it('when paused', () => {
    const consumer = new ConsumerBackPressure({});
    consumer.isPaused = true;
    consumer.consumer = new KafkaMock.KafkaConsumer();

    consumer.resumeConsumer();

    expect(consumer.consumer.resume).toHaveBeenCalledTimes(1);
    expect(consumer.isPaused).toBeFalsy();
  });

  it('when is not paused', () => {
    const consumer = new ConsumerBackPressure({});
    consumer.consumer = new KafkaMock.KafkaConsumer();

    consumer.resumeConsumer();

    expect(consumer.consumer.resume).toHaveBeenCalledTimes(0);
    expect(consumer.isPaused).toBeFalsy();
  });
});

test('revoke partitions', () => {
  const consumer = new ConsumerBackPressure({});
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

test('revoke partitions: paused case', () => {
  const consumer = new ConsumerBackPressure({});
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
