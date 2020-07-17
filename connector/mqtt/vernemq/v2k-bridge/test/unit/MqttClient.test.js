const mockProcess = require('jest-mock-process');

jest.mock('fs');
jest.mock('@dojot/microservice-sdk');
jest.mock('../../app/AgentMessenger');

/**
 * Mocks
 */
const fakeMqttConfig = {
  'backpressure.handlers': 1,
  'backpressure.queue.length.max': 10,
  'client.keepalive': 0,
  'client.id': 'fake',
  'client.username': 'fake',
  'server.address': 'fake',
  'server.port': 0,
  'tls.ca.file': 'fake',
  'tls.certificate.file': 'fake',
  'tls.key.file': 'fake',
};

const mockConfig = {
  fakeMqtt: {
    on: jest.fn(),
    reconnect: jest.fn(),
    subscribe: jest.fn(),
    end: jest.fn(),
  },
};

const mockAsyncQueue = {
  drain: jest.fn((callback) => callback()),
  push: jest.fn((data, callback) => callback()),
};

/**
 * Manual mocks
 */
jest.mock('mqtt', () => ({
  connect: jest.fn(() => mockConfig.fakeMqtt),
}));

jest.mock('async', () => ({
  queue: (callback) => {
    callback(jest.fn(), jest.fn());
    return mockAsyncQueue;
  },
}));

const mqtt = require('mqtt');
const { mqtt: mqttConfig } = require('../../app/config');
const MQTTClient = require('../../app/MQTTClient');
const AgentMessenger = require('../../app/AgentMessenger');

describe('Testing v2k bridge client', () => {
  let mockExit;

  beforeEach(() => {
    mockExit = mockProcess.mockProcessExit();
    jest.clearAllMocks();
  });

  afterAll(() => {
    mockExit.mockRestore();
  });

  const expectClientInitialization = (client, agent, config = mqttConfig) => {
    expect(client.config).toEqual(config);
    expect(client.isConnected).toEqual(false);

    expect(client.clientId).toEqual(config['client.id']);
    expect(client.username).toEqual(config['client.username']);
    expect(client.host).toEqual(config['server.address']);
    expect(client.keepAlive).toEqual(config['client.keepalive']);
    expect(client.agentMessenger).toEqual(agent);

    expect(client.privateKey).not.toBeNull();
    expect(client.clientCrt).not.toBeNull();
    expect(client.ca).not.toBeNull();

    expect(client.messageQueue).toBeNull();
    expect(client.currentMessageQueueLength).toEqual(0);
  };

  it('Should create a client sucessfully', () => {
    const agent = new AgentMessenger(fakeMqttConfig);
    const client = new MQTTClient(agent, fakeMqttConfig);
    expectClientInitialization(client, agent, fakeMqttConfig);
  });

  it('Should create a client sucessfully without config', () => {
    const agent = new AgentMessenger(fakeMqttConfig);
    const client = new MQTTClient(agent);
    expectClientInitialization(client, agent);
  });

  it('Should init sucessfully the client', () => {
    const agent = new AgentMessenger(fakeMqttConfig);
    const client = new MQTTClient(agent, fakeMqttConfig);
    const connectSpy = jest.spyOn(client, 'connect');
    const asyncQueueWorkerSpy = jest.spyOn(client, 'asyncQueueWorker');

    // call twice to test if condition
    client.init();
    client.isConnected = true;
    client.secureMode = true;
    client.init();

    expect(asyncQueueWorkerSpy).toHaveBeenCalled();
    expect(connectSpy).toHaveBeenCalled();
    expect(mockConfig.fakeMqtt.on).toHaveBeenCalled();
    expect(mockAsyncQueue.drain).toHaveBeenCalled();
  });

  it('Should connect the client (callback)', () => {
    const agent = new AgentMessenger(fakeMqttConfig);
    const client = new MQTTClient(agent, fakeMqttConfig);
    client.init();
    const subscribeSpy = jest.spyOn(client, 'subscribe');

    // insert two times to test the if condition
    client.onConnect();
    client.onConnect();
    expect(subscribeSpy).toHaveBeenCalledTimes(2);
    expect(client.isConnected).toEqual(true);
  });

  it('Should connect the client mqtt', () => {
    const agent = new AgentMessenger(fakeMqttConfig);
    const client = new MQTTClient(agent, fakeMqttConfig);
    client.secureMode = true;
    client.connect();
    client.isConnected = true;
    client.connect();
    expect(mqtt.connect).toHaveBeenCalledTimes(1);
    expect(mqtt.connect).toHaveBeenCalledWith(client.mqttOptions);
  });

  it('Should reconnect the client after disconnect', () => {
    const agent = new AgentMessenger(fakeMqttConfig);
    const client = new MQTTClient(agent, fakeMqttConfig);

    client.init();
    client.onDisconnect();

    expect(mockConfig.fakeMqtt.reconnect).toHaveBeenCalled();
  });

  it('Should exit after an error', () => {
    const agent = new AgentMessenger(fakeMqttConfig);
    const client = new MQTTClient(agent, fakeMqttConfig);

    client.init();
    client.onError('fake');

    expect(mockExit).toHaveBeenCalled();
  });

  it('should push a message to the queue (callback) once', () => {
    const agent = new AgentMessenger(fakeMqttConfig);
    const client = new MQTTClient(agent, fakeMqttConfig);

    client.init();
    client.isConnected = true;
    client.onMessage('any', 'any', { dup: false });
    client.onMessage('any', 'any', { dup: true });
    expect(mockAsyncQueue.push).toHaveBeenCalledTimes(1);
  });

  it('should not push a message to the queue (callback)', () => {
    const agent = new AgentMessenger(fakeMqttConfig);
    const client = new MQTTClient(agent, fakeMqttConfig);
    client.init();
    client.onMessage();
    client.currentMessageQueueLength = 10 * 1000 * 10000;
    client.onMessage();

    expect(mockAsyncQueue.push).not.toHaveBeenCalled();
    expect(mockConfig.fakeMqtt.end).toHaveBeenCalled();
  });

  it('should not subscribe to topic', () => {
    const agent = new AgentMessenger(fakeMqttConfig);
    const client = new MQTTClient(agent, fakeMqttConfig);
    client.init();
    client.isConnected = false;
    client.subscribe();
    expect(mockConfig.fakeMqtt.subscribe).not.toHaveBeenCalled();
  });

  it('should subscribe to topic', () => {
    const agent = new AgentMessenger(fakeMqttConfig);
    const client = new MQTTClient(agent, fakeMqttConfig);
    client.init();
    client.isConnected = true;
    client.subscribe();
    expect(mockConfig.fakeMqtt.subscribe).toHaveBeenCalled();
  });

  it('The agent should send message when asyncQueueWorker runs', async () => {
    const agent = new AgentMessenger(fakeMqttConfig);
    const client = new MQTTClient(agent, fakeMqttConfig);
    client.init();
    const fakeMessage = { topic: 'topic', message: '{ "name":"John", "age":30, "city":"New York"}' };
    const { topic, message } = fakeMessage;
    client.asyncQueueWorker(fakeMessage);
    expect(client.agentMessenger.sendMessage).toHaveBeenCalledWith(topic, message);
  });
});
