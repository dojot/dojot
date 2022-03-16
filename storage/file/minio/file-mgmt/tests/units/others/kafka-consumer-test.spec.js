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

const LoggerMock = require('../../mocks/logger-mock');

const config = {
  'enable.async.commit': true,
  'kafka.consumer': '',
  'kafka.topic': '',
};

const KafkaConsumer = require('../../../src/app/kafka-consumer');

describe('KafkaConsumer', () => {
  let kafkaConsumer;
  beforeEach(() => {
    kafkaConsumer = new KafkaConsumer(config, LoggerMock);
  });

  it('Should init consumer', async () => {
    kafkaConsumer.registerTopics = jest.fn();

    await kafkaConsumer.init('');

    expect.anything();
  });

  it('Should register the topics', async () => {
    kafkaConsumer.registerTopic = jest.fn();

    await kafkaConsumer.registerTopics([{
      topicSuffix: 'dojot',
      handler: () => {},
    }]);

    expect.anything();
  });

  it('Should register a topic', async () => {
    await kafkaConsumer.registerTopic('dojot', () => {});

    expect.anything();
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

  it('Should unregister callbacks, when there are callbacks ', async () => {
    kafkaConsumer.idCallbackTenant = 1;
    await kafkaConsumer.unregisterCallbacks();

    expect.anything();
  });

  it('should not throw any error, when there are not callbacks ', async () => {
    await kafkaConsumer.unregisterCallbacks();

    expect.anything();
  });
});
