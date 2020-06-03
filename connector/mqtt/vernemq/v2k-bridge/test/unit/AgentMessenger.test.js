/* eslint-disable no-param-reassign */
/* eslint-disable no-unused-vars */
jest.mock('../../app/utils');
jest.mock('../../app/MqttClient.js');

const mockProcess = require('jest-mock-process');
const utils = require('../../app/utils');
const AgentMessenger = require('../../app/AgentMessenger');

const mockExit = mockProcess.mockProcessExit();

let mockShouldResolve;
let resolveMock;
let rejectMock;

// MOCKS
const mockConfig = {
  Producer: {
    connect: jest.fn(),
    produce: jest.fn((topic, message, key) => new Promise((resolve, reject) => {
      resolve = jest.fn(resolve);
      reject = jest.fn(reject);
      resolveMock = resolve;
      rejectMock = reject;

      if (mockShouldResolve) {
        resolve();
      } else {
        reject(new Error('testError'));
      }
    })),
  },

  Logger: {
    info: jest.fn(),
    error: jest.fn(),
    debug: jest.fn(),
  },

  kafkaConfig: {
    test: 'testmock',
    messenger: {
      kafka: {
        dojot: {
          subjects: {
            verne: 'verne',
          },
        },
      },
    },
    mqtt: {
      subscribeTopic: 'topic',
    },
  },

  mqttConfig: {
    client: 'test',
    subscribe: jest.fn(),
  },

};

jest.mock('@dojot/microservice-sdk', () => ({
  Kafka: {
    Producer: jest.fn(() => mockConfig.Producer),
  },
  Logger: jest.fn(() => mockConfig.Logger),
}));

let mockedProducer;

async function expectConfigs() {
  mockConfig.Producer.connect.mockReturnValue(Promise.resolve());

  await mockedProducer.init();

  expect(mockConfig.Producer.connect).toHaveBeenCalled();
  expect(mockedProducer.mqttClient.init).toHaveBeenCalled();
  expect(mockedProducer.logger).toBeDefined();
}

describe('Testing AgentMessenger messenger', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  afterAll(() => {
    mockExit.mockRestore();
  });

  it('Should init correctly the agent messenger with config and publish data', () => {
    mockedProducer = new AgentMessenger(mockConfig.kafkaConfig);
    expectConfigs();
  });

  it('Should init correctly the agent messenger without config', () => {
    mockedProducer = new AgentMessenger();

    expectConfigs();
  });

  it('Should not init correctly the agent messenger', async () => {
    const reason = 'error';
    mockedProducer = new AgentMessenger(mockConfig.kafkaConfig);
    mockConfig.Producer.connect.mockReturnValue(Promise.reject(reason));

    try {
      await mockedProducer.init();
    } catch (error) {
      expect(error).toEqual(reason);
    }
  });

  it('Should send message', () => {
    mockedProducer = new AgentMessenger(mockConfig.kafkaConfig);
    mockShouldResolve = true;
    expectConfigs();

    const generateFakeData = {
      metadata: {
        tenant: 'fake',
        deviceid: 'fake',
      },
    };
    utils.generateDojotDeviceDataMessage = jest.fn().mockReturnValue(generateFakeData);

    const fakeMessage = '{ "name":"John", "age":30, "city":"New York"}';
    mockedProducer.sendMessage('test', fakeMessage);
    expect(mockedProducer.producer.produce).toHaveBeenCalled();
    expect(resolveMock).toHaveBeenCalled();
  });

  it('Should not send message', () => {
    mockedProducer = new AgentMessenger(mockConfig.kafkaConfig);
    mockShouldResolve = false;
    expectConfigs();

    const generateFakeData = {
      metadata: {
        tenant: 'fake',
        deviceid: 'fake',
      },
    };
    utils.generateDojotDeviceDataMessage = jest.fn().mockReturnValue(generateFakeData);

    const fakeMessage = '{ "name":"John", "age":30, "city":"New York"}';
    mockedProducer.sendMessage('test', fakeMessage);
    expect(mockedProducer.producer.produce).toHaveBeenCalled();
    expect(rejectMock).toHaveBeenCalled();
  });

  it('Should not send malformed message', () => {
    mockedProducer = new AgentMessenger(mockConfig.kafkaConfig);
    expectConfigs();

    const generateFakeData = {
      metadata: {
        tenant: 'fake',
        deviceid: 'fake',
      },
    };
    utils.generateDojotDeviceDataMessage = jest.fn().mockReturnValue(generateFakeData);
    const fakeMessage = 'error format';

    try {
      mockedProducer.sendMessage('test', fakeMessage);
    } catch (error) {
      expect(error).toBeDefined();
    } finally {
      expect(mockConfig.Producer.produce).not.toHaveBeenCalled();
    }
  });
});
