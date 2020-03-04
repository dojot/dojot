const { logger } = require('@dojot/dojot-module-logger');
const fs = require('fs');
const mqtt = require('mqtt');

const defaultConfig = require('./config');
const AgentMessenger = require('./AgentMesenger');

const TAG = { filename: 'MqttClient' };

class MqttClient {
  constructor(config) {
    this.config = config || defaultConfig;
    this.isConnected = false;

    this.clientId = this.config.mqtt.clientId;
    this.host = this.config.mqtt.host;
    this.keepAlive = this.config.mqtt.keepAlive;
    this.port = this.config.mqtt.port;
    this.username = this.config.mqtt.clientUsername;
    this.secureMode = this.config.mqtt.secure;
    this.publishQos = this.config.mqtt.publishQos;

    this.privateKey = fs.readFileSync(`${this.config.mqtt.tls.privateKey.location}`);
    this.clientCrt = fs.readFileSync(`${this.config.mqtt.tls.certificate.location}`);
    this.ca = fs.readFileSync(`${this.config.mqtt.tls.ca.location}`);

    this.mqttc = null;
    this.mqttOptions = null;
    this.agentMessenger = null;
  }

  /**
   * Initializes the MQTT connection and the callbacks used by the client.
   */
  init() {
    logger.info('Connecting MQTT client...', TAG);

    // create the agent messenger
    this.agentMessenger = new AgentMessenger(this, this.config);

    this.mqttOptions = {
      username: this.username,
      clientId: this.clientId,
      host: this.host,
      port: this.port,
      protocol: this.secureMode ? 'mqtts' : 'mqtt',
      ca: this.ca,
      key: this.privateKey,
      cert: this.clientCrt,
      keepAlive: this.keepAlive,
      clean: false,
      rejectUnauthorized: true,
    };

    this.mqttc = mqtt.connect(this.mqttOptions);

    const mqttOnConnectBind = this.mqttOnConnect.bind(this);
    const mqttOnDisconnectBind = this.mqttOnDisconnect.bind(this);

    logger.info('Binding event callbacks', TAG);
    this.mqttc.on('connect', mqttOnConnectBind);
    this.mqttc.on('disconnect', mqttOnDisconnectBind);
  }

  /* MQTT Events */

  mqttOnConnect() {
    logger.info('MQTT connection established', TAG);
    this.isConnected = true;
    this.agentMessenger.init();
  }

  mqttOnDisconnect() {
    this.isConnected = false;
    logger.info('MQTT connection ended, trying to reconnect...', TAG);
    this.mqttc.reconnect();
    // TODO: agentMessenger (.pause or .stop)
  }

  /* MQTT Functions */

  /**
   * Publish a MQTT message.
   * @param topic
   * @param message
   */
  publishMessage(topic, message) {
    if (this.isConnected && this.mqttc) {
      logger.debug(`Publishing on topic ${topic}`, TAG);
      this.mqttc.publish(topic, message, { qos: this.publishQos });
    } else {
      logger.error('Client not connected', TAG);
    }
  }
}

module.exports = MqttClient;
