const fs = require('fs');
const path = require('path');
const mqtt = require('mqtt');
const uuid4 = require('uuid/v4');
const { logger } = require('@dojot/dojot-module-logger');
const defaultConfig = require('./config');

const TAG = { filename: 'MqttClient' };
const AgentMessenger = require('./AgentMessenger');

class MQTTClient {
  constructor(config) {
    this.config = config || defaultConfig;
    this.isConnected = false;
    this.agentMessenger = null;

    this.clientId = `${this.config.mqtt.clientId}-${uuid4()}`;
    this.username = this.config.mqtt.clientUsername;

    this.key = fs.readFileSync(path.join(__dirname, `${this.config.mqtt.hostname}.key`));
    this.clientCrt = fs.readFileSync(path.join(__dirname, `${this.config.mqtt.hostname}.crt`));
    this.caCrt = fs.readFileSync(path.join(__dirname, 'ca.crt'));
  }

  init() {
    const mqttOptions = {
      username: this.username,
      clientId: this.clientId,
      host: this.config.mqtt.host,
      port: this.config.mqtt.port,
      protocol: 'mqtts',
      ca: this.caCrt,
      key: this.key,
      cert: this.clientCrt,
      keepAlive: this.config.mqtt.keepAlive,
      clean: false,
      rejectUnauthorized: true,
    };

    const onConnectBind = this.onConnect.bind(this);
    const onDisconnectBind = this.onDisconnect.bind(this);
    const onMessageBind = this.onMessage.bind(this);

    this.mqttc = mqtt.connect(mqttOptions);
    this.mqttc.on('connect', onConnectBind);
    this.mqttc.on('disconnect', onDisconnectBind);
    this.mqttc.on('message', onMessageBind);
  }

  onConnect() {
    this.isConnected = true;
    logger.info(`Client ${this.clientId} connected successfully!`, TAG);

    if (this.agentMessenger === null) {
      this.agentMessenger = new AgentMessenger(this.config);
      this.agentMessenger.init(this.mqttc);
    }
  }

  onDisconnect() {
    this.isConnected = false;
    this.mqttc.reconnect();
  }

  onMessage(topic, message) {
    this.agentMessenger.sendMessage(topic, message);
  }
}

module.exports = MQTTClient;
