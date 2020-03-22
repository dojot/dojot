/**
 * file: MqttClient.test.js
 * dependencies: [
 *                 @dojot/dojot-module,
 *                 @dojot/iotagent-nodejs,
 *                 @dojot/dojot-module-logger,
 *                 fs
 *                ]
 * Author: dojot
 */

/**
 * Dependencies mock
 */
jest.mock('fs');
jest.mock('@dojot/dojot-module');
jest.mock('@dojot/dojot-module-logger');
jest.mock('@dojot/iotagent-nodejs');
jest.mock('../../app/AgentMessenger');

/**
 * Mocks
 */
const fakeConfig = {
  mqtt: {
    clientUsername: 'fake',
    clientId: 'fake',
    host: 'fake',
    port: 0,
    keepAlive: 0,
    tls: {
      ca: {
        location: 'fake',
      },
      certificate: {
        location: 'fake',
      },
      privateKey: {
        location: 'fake',
      },
    },
  },
  app: {
    mqttLogLevel: 'debug',
    baseDir: 'fakeDir',
    hostname: 'fake',
  },

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
const defaultConfig = require('../../app/config');
const MQTTClient = require('../../app/MqttClient');
const AgentMessenger = require('../../app/AgentMessenger');

describe('Testing v2k bridge client', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  const expectClientInitialization = (client, agent, config = defaultConfig) => {
    expect(client.config).toEqual(config);
    expect(client.isConnected).toEqual(false);

    expect(client.clientId).toEqual(config.mqtt.clientId);
    expect(client.username).toEqual(config.mqtt.clientUsername);
    expect(client.host).toEqual(config.mqtt.host);
    expect(client.keepAlive).toEqual(config.mqtt.keepAlive);
    expect(client.hostname).toEqual(config.app.hostname);
    expect(client.agentMessenger).toEqual(agent);

    expect(client.privateKey).not.toBeNull();
    expect(client.clientCrt).not.toBeNull();
    expect(client.ca).not.toBeNull();

    expect(client.messageQueue).toBeNull();
    expect(client.currentMessageQueueLenght).toEqual(0);
  };

  it('Should create a client sucessfully', () => {
    const agent = new AgentMessenger(fakeConfig);
    const client = new MQTTClient(agent, fakeConfig);
    expectClientInitialization(client, agent, fakeConfig);
  });

  it('Should create a client sucessfully without config', () => {
    const agent = new AgentMessenger(fakeConfig);
    const client = new MQTTClient(agent);
    expectClientInitialization(client, agent);
  });

  it('Should init sucessfully the client', () => {
    const agent = new AgentMessenger(fakeConfig);
    const client = new MQTTClient(agent, fakeConfig);
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
    const agent = new AgentMessenger(fakeConfig);
    const client = new MQTTClient(agent, fakeConfig);
    client.init();
    const subscribeSpy = jest.spyOn(client, 'subscribe');

    // insert two times to test the if condition
    client.onConnect();
    client.onConnect();
    expect(subscribeSpy).toHaveBeenCalledTimes(2);
    expect(client.isConnected).toEqual(true);
  });

  it('Should connect the client mqtt', () => {
    const agent = new AgentMessenger(fakeConfig);
    const client = new MQTTClient(agent, fakeConfig);
    client.connect();
    client.isConnected = true;
    client.connect();
    expect(mqtt.connect).toHaveBeenCalledTimes(1);
    expect(mqtt.connect).toHaveBeenCalledWith(client.mqttOptions);
  });

  it('Should reconnect the client after disconnect', () => {
    const agent = new AgentMessenger(fakeConfig);
    const client = new MQTTClient(agent, fakeConfig);

    client.init();
    client.onDisconnect();

    expect(mockConfig.fakeMqtt.reconnect).toHaveBeenCalled();
  });

  it('should push a message to the queue (callback)', () => {
    const agent = new AgentMessenger(fakeConfig);
    const client = new MQTTClient(agent, fakeConfig);

    client.init();
    client.isConnected = true;
    client.onMessage();

    expect(mockAsyncQueue.push).toHaveBeenCalledTimes(1);
  });

  it('should not push a message to the queue (callback)', () => {
    const agent = new AgentMessenger(fakeConfig);
    const client = new MQTTClient(agent, fakeConfig);
    client.init();
    client.onMessage();
    client.currentMessageQueueLenght = 10 * 1000 * 10000;
    client.onMessage();

    expect(mockAsyncQueue.push).not.toHaveBeenCalled();
    expect(mockConfig.fakeMqtt.end).toHaveBeenCalled();
  });

  it('should not subscribe to topic', () => {
    const agent = new AgentMessenger(fakeConfig);
    const client = new MQTTClient(agent, fakeConfig);
    client.init();
    client.isConnected = false;
    client.subscribe();
    expect(mockConfig.fakeMqtt.subscribe).not.toHaveBeenCalled();
  });

  it('should subscribe to topic', () => {
    const agent = new AgentMessenger(fakeConfig);
    const client = new MQTTClient(agent, fakeConfig);
    client.init();
    client.isConnected = true;
    client.subscribe();
    expect(mockConfig.fakeMqtt.subscribe).toHaveBeenCalled();
  });

  it('The agent should send message when asyncQueueWorker runs', async () => {
    const agent = new AgentMessenger(fakeConfig);
    const client = new MQTTClient(agent, fakeConfig);
    client.init();
    const fakeMessage = { topic: 'topic', message: '{ "name":"John", "age":30, "city":"New York"}' };
    const { topic, message } = fakeMessage;
    client.asyncQueueWorker(fakeMessage);
    expect(client.agentMessenger.sendMessage).toHaveBeenCalledWith(topic, message);
  });
});
