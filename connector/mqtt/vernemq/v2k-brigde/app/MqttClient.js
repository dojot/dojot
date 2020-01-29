const fs = require('fs');
const mqtt = require('mqtt');
const { logger } = require('@dojot/dojot-module-logger');
const defaultConfig = require('./config');

const TAG = { filename: 'mqtt-client' };
const AgentMessenger = require('./AgentMessenger');

class MQTTClient {
  constructor(config) {
    this.config = config || defaultConfig;
    this.isConnected = false;
    this.agentMessenger = null;

    this.key = fs.readFileSync(`/opt/v2k_bridge/cert/${this.config.mqtt.mqttHost}.key`);
    this.clientCrt = fs.readFileSync(`/opt/v2k_bridge/cert/${this.config.mqtt.mqttHost}.crt`);
    this.caCrt = fs.readFileSync('/opt/v2k_bridge/cert/ca.crt');

    /* set log level */
    logger.setLevel(this.config.app.mqtt_log_level);
  }

  init() {
    const mqttOptions = {
      username: this.config.mqtt.mqttHost,
      clientId: this.config.mqtt.mqttHost,
      host: this.config.mqtt.host,
      port: this.config.mqtt.port,
      protocol: 'mqtts',
      ca: this.caCrt,
      key: this.key,
      cert: this.clientCrt,
      keepAlive: this.config.mqtt.keepAlive,
      rejectUnauthorized: false,
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
    logger.info('Client Connected successfully!', TAG);

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
