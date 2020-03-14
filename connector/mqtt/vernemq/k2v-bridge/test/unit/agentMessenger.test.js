/**
 * Unit test for AgentMessenger file
 *
 * This module has the following dependencies
 *
 * - @dojot/iotagent-nodejs
 */

const AgentMessenger = require('../../app/AgentMessenger');
const MqttClient = require('../../app/MqttClient');
const ProjectUtils = require('../../app/utils/utils');

const mockDefaultConfig = {
  mqtt: {
    publishTopicSuffix: '/fake',
  },
};

const fakeArg = (arg1) => ({ meta: { service: arg1 }, data: { id: arg1, attrs: { atr1: 'attr1' } } });

const mockConfig = {
  Messenger: {
    updateAttrs: jest.fn(),
    init: jest.fn(),
    on: jest.fn((arg0, arg1, callback) => callback(arg0, fakeArg(arg1))),
  },
  kafkaConfig: {
    test: 'testMock',
    messenger: {
      kafka: {
        dojot: {
          subject: {
            verne: 'verne',
          },
        },
      },
    },
  },
};

jest.mock('@dojot/iotagent-nodejs', () => ({
  IoTAgent: jest.fn(() => mockConfig.Messenger),
}));

jest.mock('../../app/utils/utils', () => ({
  generateDojotActuationTopic: jest.fn(() => 'abc'),
}));

jest.mock('../../app/MqttClient', () => jest.fn(() => ({
  publishMessage: jest.fn(),
})));

describe('Test AgentMessenger', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('should create an agent messenger successfully without config', () => {
    const mqttClient = new MqttClient();
    const agentMessenger = new AgentMessenger(mqttClient);

    expect(agentMessenger.config).toBeDefined();
    expect(agentMessenger.mqttClient).toEqual(mqttClient);
    expect(agentMessenger.iotagent).toEqual(null);
  });

  it('should create an agent messenger successfully with a config', () => {
    const mqttClient = new MqttClient();
    const agentMessenger = new AgentMessenger(mqttClient, mockDefaultConfig);

    expect(agentMessenger.config).toEqual(mockDefaultConfig);
    expect(agentMessenger.mqttClient).toEqual(mqttClient);
    expect(agentMessenger.iotagent).toEqual(null);
  });

  it('should init successfully the Agent Messenger', async () => {
    mockConfig.Messenger.init.mockReturnValue(Promise.resolve());
    const mqttClient = new MqttClient();
    const agentMessenger = new AgentMessenger(mqttClient, mockDefaultConfig);
    await agentMessenger.init();
    expect(ProjectUtils.generateDojotActuationTopic).toHaveBeenCalledTimes(1);
    expect(agentMessenger.mqttClient.publishMessage).toHaveBeenCalledTimes(1);
  });

  it('should fail initializing the Agent Messenger', async () => {
    mockConfig.Messenger.init.mockReturnValue(Promise.reject());
    const mqttClient = new MqttClient();
    const agentMessenger = new AgentMessenger(mqttClient, mockDefaultConfig);
    await agentMessenger.init();
    expect(ProjectUtils.generateDojotActuationTopic).not.toHaveBeenCalled();
    expect(agentMessenger.mqttClient.publishMessage).not.toHaveBeenCalled();
  });
});
