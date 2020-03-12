const fs = require('fs');
const async = require('async');
const mqtt = require('mqtt');
const { logger } = require('@dojot/dojot-module-logger');
const defaultConfig = require('./config');

const TAG = { filename: 'MqttClient' };
/**
 * Class representing an MQTTClient
 * @class
 */
class MQTTClient {
  /**
   * Create an MQTTClient
   * @param {Object} agentMessenger - the client agent messenger
   * @param {*} config - the client configuration
   */
  constructor(agentMessenger, config) {
    this.config = config || defaultConfig;
    this.isConnected = false;

    this.clientId = this.config.mqtt.clientId;
    this.host = this.config.mqtt.host;
    this.keepAlive = this.config.mqtt.keepAlive;
    this.port = this.config.mqtt.port;
    this.username = this.config.mqtt.clientUsername;
    this.secureMode = this.config.mqtt.secure;

    this.privateKey = fs.readFileSync(`${this.config.mqtt.tls.privateKey.location}`);
    this.clientCrt = fs.readFileSync(`${this.config.mqtt.tls.certificate.location}`);
    this.ca = fs.readFileSync(`${this.config.mqtt.tls.ca.location}`);

    // backPressure
    this.messageQueue = null;
    this.currentMessageQueueLength = 0;
    this.parallelHandlers = this.config.mqtt.parallelHandlers;
    this.maxQueueLength = this.config.mqtt.maxQueueLength;

    // agent messenger
    this.agentMessenger = agentMessenger;
  }

  /**
   * @function init
   * Initialize the mqttClient loading it's attributes, registering
   * it's callbacks and connecting to a broker
   */
  init() {
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

    const onConnectBind = this.onConnect.bind(this);
    const onDisconnectBind = this.onDisconnect.bind(this);
    const onMessageBind = this.onMessage.bind(this);

    this.connect();
    this.mqttc.on('connect', onConnectBind);
    this.mqttc.on('disconnect', onDisconnectBind);
    this.mqttc.on('message', onMessageBind);

    /**
     * Create async queue
     */
    this.messageQueue = async.queue((data, done) => {
      this.asyncQueueWorker(data);
      done();
    }, this.parallelHandlers);

    /**
     * When processing was finalized, reconnect to broker
     */
    this.messageQueue.drain(() => {
      if (this.isConnected === false) {
        this.mqttc.reconnect();
      }
    });
  }

  /**
   * This function is reached when the MQTTClient
   * connect successfully to the broker.
   * @callback MqttClient~onConnect
   */
  onConnect() {
    this.isConnected = true;
    logger.info(`Client ${this.clientId} connected successfully!`, TAG);
    this.subscribe();
  }

  /**
   * Reached when the MQTTClient disconnect from the broker.
   * @callback MqttClient~onDisconnect
   */
  onDisconnect() {
    logger.info(`Client ${this.clientId} disconnected, reconnecting ......`);
    this.isConnected = false;
    this.mqttc.reconnect();
  }

  /**
   * Reached when a message arrive on the topic
   * @callback MqttClient~onMessage
   * @param {string} topic
   * @param {Object} message
   * @param {Object} packet
   */
  onMessage(topic, message, packet) {
    // pause
    if (this.isConnected) {
      // check if message is duplicated
      const size = message.toString().length;
      if (packet.dup === false) {
        this.currentMessageQueueLength += size;
        const data = { topic, message };
        this.messageQueue.push(data, () => {
          this.currentMessageQueueLength -= size;
        });
      }
    }

    if (this.currentMessageQueueLength > this.maxQueueLength) {
      this.mqttc.end(true);
      this.isConnected = false;
    }
  }

  /**
   * @function connect
   * Connect the MQTTClient to the broker
   */
  connect() {
    if (this.isConnected === false) {
      logger.debug(`Connecting to broker ${this.host} on port ${this.port} with protocol ${this.secureMode ? 'mqtts' : 'mqtt'}`, TAG);
      this.mqttc = mqtt.connect(this.mqttOptions);
    }
  }

  /**
   * @function subscribe
   * Subscribe the client to it's topics
   */
  subscribe() {
    logger.info(`Subscribing to topic ${this.config.mqtt.subscribeTopic}`, TAG);
    if (this.isConnected === true) {
      this.mqttc.subscribe(this.config.mqtt.subscribeTopic, { qos: this.config.mqtt.subscribeQos });
    }
  }

  /**
   * @private
   * @function asyncQueueWorker
   * @param {Object} data
   */
  asyncQueueWorker(data) {
    const { topic, message } = data;
    this.agentMessenger.sendMessage(topic, message);
  }
}

module.exports = MQTTClient;
