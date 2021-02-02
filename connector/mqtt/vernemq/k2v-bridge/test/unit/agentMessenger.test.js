const AgentMessenger = require('../../app/AgentMessenger');
const MQTTClient = require('../../app/MqttClient');

const mockConfig = {
  Kafka: {
    Consumer: {
      init: jest.fn(),
      registerCallback: jest.fn(),
    },
  },
  Logger: {
    info: jest.fn(),
    error: jest.fn(),
  },
};

jest.mock('../../app/config', () => ({
  kafka: { },
  messenger: {
    'consume.topic.suffix': '/fake',
  },
  sdk: { },
}));

jest.mock('../../app/MqttClient', () => jest.fn(() => ({
  publishMessage: jest.fn(),
})));

jest.mock('@dojot/microservice-sdk', () => ({
  Kafka: {
    Consumer: jest.fn(() => mockConfig.Kafka.Consumer),
  },
  Logger: jest.fn(() => mockConfig.Logger),
}));

jest.mock('../../app/utils', () => ({
  killApplication: jest.fn(),
}));

describe('Test AgentMessenger', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('should successfully create an Agent Messenger', () => {
    const mqttClient = new MQTTClient();
    const agentMessenger = new AgentMessenger(mqttClient);

    expect(agentMessenger.mqttClient).toEqual(mqttClient);
    expect(agentMessenger.consumer).toBeDefined();
  });

  it('should successfully initialize the Agent Messenger', async () => {
    mockConfig.Kafka.Consumer.init.mockReturnValue(Promise.resolve());
    const mqttClient = new MQTTClient();
    const agentMessenger = new AgentMessenger(mqttClient);
    await agentMessenger.init();
    expect(mockConfig.Kafka.Consumer.registerCallback).toHaveBeenCalledTimes(1);
  });

  it('should fail the initialization of the Agent Messenger', async () => {
    mockConfig.Kafka.Consumer.init.mockReturnValue(Promise.reject(new Error('fakeError')));
    const mqttClient = new MQTTClient();
    const agentMessenger = new AgentMessenger(mqttClient);
    await agentMessenger.init();

    expect(agentMessenger.mqttClient.publishMessage).not.toHaveBeenCalled();
  });

  it('should send a message when the registered callback is called', async () => {
    mockConfig.Kafka.Consumer.init.mockReturnValue(Promise.resolve());
    const mqttClient = new MQTTClient();
    const agentMessenger = new AgentMessenger(mqttClient);
    await agentMessenger.init();
    expect(mockConfig.Kafka.Consumer.registerCallback).toHaveBeenCalledTimes(1);

    // Retrieving the callback passed to registerCallback
    const callback = mockConfig.Kafka.Consumer.registerCallback.mock.calls[0][1];
    callback({});
    expect(agentMessenger.mqttClient.publishMessage).toHaveBeenCalledTimes(1);
  });
});
