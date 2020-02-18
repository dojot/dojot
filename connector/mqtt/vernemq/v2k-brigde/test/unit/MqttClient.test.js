jest.mock('@dojot/dojot-module');
jest.mock('@dojot/dojot-module-logger');
jest.mock('fs');

const fakeConfig = {
  mqtt: {
    mqttHost: 'fake',
    host: 'fake',
    port: 'fake',
    keepAlive: 0,
  },
  app: {
    mqttLogLevel: 'debug',
  },

};

const mockConfig = {
  fakeMqtt: {
    on: jest.fn(),
    reconnect: jest.fn(),
  },
};

const mqtt = require('mqtt');

jest.mock('@dojot/iotagent-nodejs');

jest.mock('mqtt', () => ({
  connect: jest.fn(() => mockConfig.fakeMqtt),
}));

jest.mock('../../app/AgentMessenger');

const MQTTClient = require('../../app/MqttClient');

describe('Testing v2k bridge client', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('Should init sucessfully the client', () => {
    const client = new MQTTClient(fakeConfig);
    client.init();

    expect(mqtt.connect).toHaveBeenCalled();
    expect(mockConfig.fakeMqtt.on).toHaveBeenCalled();
  });

  it('Should init sucessfully the client without conf', () => {
    const client = new MQTTClient();
    client.init();

    expect(mqtt.connect).toHaveBeenCalled();
    expect(mockConfig.fakeMqtt.on).toHaveBeenCalled();
  });


  it('Should connect the client and init agent', () => {
    const client = new MQTTClient(fakeConfig);
    client.init();
    // insert two times to test the if condition
    client.onConnect();
    client.onConnect();

    expect(client.agentMessenger.init).toHaveBeenCalledTimes(1);
  });

  it('Should reconnect the client after disconnect', () => {
    const client = new MQTTClient(fakeConfig);
    client.init();

    client.onDisconnect();

    expect(mockConfig.fakeMqtt.reconnect).toHaveBeenCalled();
  });

  it('Should send message after onMessage', async () => {
    const client = new MQTTClient(fakeConfig);
    client.init();
    client.onConnect();
    const fakeMessage = '{ "name":"John", "age":30, "city":"New York"}';
    await client.onMessage('test', fakeMessage);
    expect(client.agentMessenger.sendMessage).toHaveBeenCalled();
  });
});
