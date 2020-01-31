const { logger } = require('@dojot/dojot-module-logger');
const { IoTAgent } = require('@dojot/iotagent-nodejs');
const fs = require('fs');
const mqtt = require('mqtt');
const ProjectUtils = require('./utils/utils');
const defaultConfig = require('./config');

const TAG = { filename: 'MqttClientApp' };

class App {
  constructor(config) {
    this.config = config || defaultConfig;
    this.key = fs.readFileSync(`/opt/k2v_bridge/cert/${this.config.app.hostname}.key`);
    this.clientCrt = fs.readFileSync(`/opt/k2v_bridge/cert/${this.config.app.hostname}.crt`);
    this.caCrt = fs.readFileSync('/opt/k2v_bridge/cert/ca.crt');
    this.mqttc = null;
    this.isConnected = false;
    this.iotagent = null;

    this.options = {
      ca: this.caCrt,
      cert: this.clientCrt,
      host: this.config.mqtt.host,
      keepAlive: 120,
      key: this.key,
      port: this.config.mqtt.port,
      protocol: 'mqtts',
      rejectUnauthorized: false,
    };
  }

  initApp() {
    /* set log level */
    logger.setLevel(this.config.app.mqtt_log_level);

    this.initMqtt();

    this.iotagent = new IoTAgent();
    this.iotagent.init().then(() => {
      /* Actuation message handler */
      this.iotagent.on('iotagent.device', 'device.configure', (tenant, event) => {
        logger.debug(`Got device actuation message. Tenant is ${tenant}.`, TAG);

        const configTopic = ProjectUtils.generateActuationTopic(event.meta.service,
          event.data.id);

        this.publishMessage(configTopic, JSON.stringify(event.data.attrs));
      });
    }).catch(() => {
      logger.error('An error occurred while initializing the IoTAgent. Bailing out!', TAG);
    });
  }

  /**
   * Initializes the MQTT connection and the callbacks used by the client.
   */
  initMqtt() {
    logger.info('Connecting MQTT client...', TAG);
    this.mqttc = mqtt.connect(this.options);

    const mqttOnConnectBind = this.mqttOnConnect.bind(this);
    const mqttOnDisconnectBind = this.mqttOnDisconnect.bind(this);

    logger.info('Binding event callbacks', TAG);
    this.mqttc.on('connect', mqttOnConnectBind);
    this.mqttc.on('disconnect', mqttOnDisconnectBind);
  }

  /* MQTT Events */

  mqttOnConnect() {
    this.isConnected = true;
    logger.info('MQTT connection established', TAG);
  }

  mqttOnDisconnect() {
    this.isConnected = false;
    logger.info('MQTT connection ended, trying to reconnect...', TAG);
    this.mqttc.reconnect();
    // TODO: close Kafka connection
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
      this.mqttc.publish(topic, message);
    } else {
      logger.error('Client not connected', TAG);
    }
  }
}

module.exports = App;
