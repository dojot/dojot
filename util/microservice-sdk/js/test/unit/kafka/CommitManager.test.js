const CommitManager = require('../../../lib/kafka/CommitManager.js');

// mocking dependencies
jest.mock('logging/Logger.js');

test('Basic initialization', async () => {
  jest.useFakeTimers();
  const commitCallback = jest.fn();
  const commitInterval = 5000;
  const commitManager = new CommitManager(commitCallback, commitInterval);
  commitManager.init();

  jest.runOnlyPendingTimers();

  expect(commitManager.caller).not.toBeNull();
  expect(setInterval).toHaveBeenCalledWith(expect.any(Function), commitInterval);
});

test('Initialization with a preconfigured caller', async () => {
  jest.useFakeTimers();
  const commitCallback = jest.fn();
  const commitInterval = 5000;
  const commitManager = new CommitManager(commitCallback, commitInterval);

  // fake caller
  const fakeInterval = 1000;
  commitManager.caller = setInterval(() => {}, fakeInterval);

  // reset caller
  commitManager.init();

  jest.runOnlyPendingTimers();

  expect(commitManager.caller).not.toBeNull();
  expect(setInterval).toHaveBeenCalledWith(expect.any(Function), fakeInterval);
  expect(setInterval).toHaveBeenCalledWith(expect.any(Function), commitInterval);
  expect(clearInterval).toHaveBeenCalledTimes(1);
});

test('start processing', async () => {
  const commitCallback = jest.fn();
  const commitInterval = 5000;
  const commitManager = new CommitManager(commitCallback, commitInterval);

  const data = {
    value: Buffer.from('konnichiwa'), // message contents as a Buffer
    size: 10, // size of the message, in bytes
    topic: 'tenant/dojot', // topic the message comes from
    offset: 1337, // offset the message was read from
    partition: 1, // partition the message was on
    key: 'someKey', // key of the message if present
    timestamp: 1510325354780, // timestamp of message creation
  };
  commitManager.notifyStartProcessing(data);

  expect(commitManager.topics).toHaveProperty(data.topic);
  expect(commitManager.topics[data.topic]).toHaveProperty(data.partition.toString());
  expect(commitManager.topics[data.topic][data.partition]).toHaveLength(1);
  expect(commitManager.topics[data.topic][data.partition][0]).toStrictEqual({
    offset: data.offset,
    done: false,
  });
});

test('finished processing', async () => {
  const commitCallback = jest.fn();
  const commitInterval = 5000;
  const commitManager = new CommitManager(commitCallback, commitInterval);

  const data = {
    value: Buffer.from('konnichiwa'), // message contents as a Buffer
    size: 10, // size of the message, in bytes
    topic: 'tenant/dojot', // topic the message comes from
    offset: 1337, // offset the message was read from
    partition: 1, // partition the message was on
    key: 'someKey', // key of the message if present
    timestamp: 1510325354780, // timestamp of message creation
  };

  commitManager.topics[data.topic] = {};
  commitManager.topics[data.topic][data.partition] = [];
  commitManager.topics[data.topic][data.partition].push({
    offset: data.offset,
    done: false,
  });

  commitManager.notifyFinishedProcessing(data);

  expect(commitManager.topics[data.topic][data.partition][0]).toStrictEqual({
    offset: data.offset,
    done: true,
  });
});

test('behavior on rebalance situations', () => {
  const commitCallback = jest.fn();
  const commitInterval = 5000;
  const commitManager = new CommitManager(commitCallback, commitInterval);

  const structure = {
    'tenant/dojot': {
      1: [
        { offset: 1000, done: true },
        { offset: 1001, done: true },
        { offset: 1002, done: false },
      ],
    },
  };

  commitManager.topics = structure;
  commitManager.onRebalance();

  expect(commitManager.topics).toStrictEqual({});
});

test('consolidates the processed messages', async () => {
  const commitCallback = jest.fn();
  const commitInterval = 5000;
  const commitManager = new CommitManager(commitCallback, commitInterval);

  const structure = {
    'tenant/dojot': {
      1: [
        { offset: 1000, done: true },
        { offset: 1001, done: true },
        { offset: 1002, done: false },
      ],
      3: [
        { offset: 2000, done: true },
        { offset: 2001, done: true },
        { offset: 2002, done: true },
      ],
    },
    'tenant/acme': {
      10: [
        { offset: 30, done: true },
      ],
    },
    'tenant/umbrella': {
      3: [
        { offset: 11, done: false },
        { offset: 12, done: true },
      ],
    },
  };
  commitManager.topics = structure;
  commitManager.commitProcessedOffsets();

  expect(commitCallback).toHaveBeenCalledWith([
    {
      topic: 'tenant/dojot',
      partition: 1,
      offset: 1002,
    },
    {
      topic: 'tenant/dojot',
      partition: 3,
      offset: 2003,
    },
    {
      topic: 'tenant/acme',
      partition: 10,
      offset: 31,
    },
  ]);

  expect(commitManager.topics).toStrictEqual({
    'tenant/dojot': {
      1: [
        { offset: 1002, done: false },
      ],
    },
    'tenant/umbrella': {
      3: [
        { offset: 11, done: false },
        { offset: 12, done: true },
      ],
    },
  });
});
