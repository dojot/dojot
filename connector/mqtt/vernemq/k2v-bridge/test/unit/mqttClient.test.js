/**
 * Unit test for MqttClient file
 *
 * This module has the following dependencies
 *
 * - mqtt
 * - fs
 */

const mqtt = require('mqtt');
const defaultConfig = require('../../app/config');
const MqttClient = require('../../app/MqttClient');

/* dependencies mock */
const mockMqtt = {
  on: jest.fn(),
  reconnect: jest.fn(),
  publish: jest.fn(),
};

jest.mock('mqtt', () => ({
  connect: jest.fn(() => mockMqtt),
  disconnect: jest.fn(),
}));

jest.mock('../../app/AgentMesenger', () => jest.fn(() => ({
  init: jest.fn(),
})));

const mockDefaultConfig = {
  mqtt: {
    clientUsername: 'fake',
    clientId: 'fake',
    host: 'mqtt-host',
    port: 0,
    keepAlive: 0,
    secure: true,
    publishTopicSuffix: '/fake',
    publishQos: 0,
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
    mqtt_log_level: 'debug',
    baseDir: 'fakeDir',
    hostname: 'fake',
  },
  toBoolean: (val) => val,
};


jest.mock('fs');
jest.mock('../../app/utils/utils');
jest.mock('@dojot/dojot-module-logger');

describe('Testing MqttClient', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  const expectMqttInitialization = (client, config = defaultConfig) => {
    expect(client.config).toEqual(config);
    expect(client.isConnected).toEqual(false);

    expect(client.clientId).toEqual(config.mqtt.clientId);
    expect(client.username).toEqual(config.mqtt.clientUsername);
    expect(client.host).toEqual(config.mqtt.host);
    expect(client.keepAlive).toEqual(config.mqtt.keepAlive);
    expect(client.hostname).toEqual(config.app.hostname);

    expect(client.privateKey).not.toBeNull();
    expect(client.clientCrt).not.toBeNull();
    expect(client.ca).not.toBeNull();
  };

  it('Should initialize the app correctly with default config', () => {
    const mqttClient = new MqttClient();
    expectMqttInitialization(mqttClient);
  });

  it('Should initialize the app correctly with mocked configs', () => {
    const mqttClient = new MqttClient(mockDefaultConfig);
    expectMqttInitialization(mqttClient, mockDefaultConfig);
  });

  it('Should initializing correctly the client with protocol mqtt', () => {
    mockDefaultConfig.mqtt.secure = false;
    const mqttClient = new MqttClient(mockDefaultConfig);
    mqttClient.init();

    expect(mqtt.connect).toHaveBeenCalledTimes(1);
    expect(mockMqtt.on).toHaveBeenCalledTimes(2);
  });

  it('Should initializing correctly the client with protocol mqtts', () => {
    mockDefaultConfig.mqtt.secure = true;
    const mqttClient = new MqttClient(mockDefaultConfig);
    mqttClient.init();

    expect(mqtt.connect).toHaveBeenCalledTimes(1);
    expect(mockMqtt.on).toHaveBeenCalledTimes(2);
  });

  it('should connect successfully the client (callback)', () => {
    const mqttClient = new MqttClient(mockDefaultConfig);
    mqttClient.init();
    const spyAgentMessengerInit = jest.spyOn(mqttClient.agentMessenger, 'init');

    mqttClient.mqttOnConnect();

    expect(mqttClient.isConnected).toEqual(true);
    expect(spyAgentMessengerInit).toHaveBeenCalledTimes(1);
  });


  it('should disconnect and reconnect successfully the client (callback)', () => {
    const mqttClient = new MqttClient(mockDefaultConfig);
    mqttClient.init();

    mqttClient.mqttOnDisconnect();

    expect(mqttClient.isConnected).toEqual(false);
    expect(mockMqtt.reconnect).toHaveBeenCalledTimes(1);
  });

  it('should publish a message', () => {
    const mqttClient = new MqttClient(mockDefaultConfig);
    mqttClient.init();
    mqttClient.mqttOnConnect();
    const fakeTopic = 'fake-topic';
    const fakeMessage = { key: 'value' };

    mqttClient.publishMessage(fakeTopic, fakeMessage);

    expect(mockMqtt.publish).toHaveBeenCalledWith(fakeTopic,
      fakeMessage,
      { qos: mockDefaultConfig.mqtt.publishQos });
  });

  it('should not publish a message', () => {
    const mqttClient = new MqttClient(mockDefaultConfig);
    mqttClient.init();
    mqttClient.publishMessage();
    expect(mockMqtt.publish).not.toHaveBeenCalled();
  });
});
