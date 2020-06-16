const fs = require('fs');
const async = require('async');
const mqtt = require('mqtt');
const { Logger } = require('@dojot/microservice-sdk');
const { mqtt: mqttConfig } = require('./config');

/**
 * Class representing an MQTTClient.
 *
 * @class MQTTClient
 */
class MQTTClient {
  /**
   * Creates an MQTTClient.
   *
   * @constructor
   * @param {Object} agentMessenger - the client agent messenger
   * @param {*} config - the client configuration
   */
  constructor(agentMessenger, config) {
    this.config = config || mqttConfig;
    this.isConnected = false;

    this.clientId = this.config['client.id'];
    this.host = this.config['server.address'];
    this.keepAlive = this.config['client.keepalive'];
    this.port = this.config['server.port'];
    this.username = this.config['client.username'];
    this.secureMode = this.config['client.secure'];

    this.privateKey = fs.readFileSync(`${this.config['tls.key.file']}`);
    this.clientCrt = fs.readFileSync(`${this.config['tls.certificate.file']}`);
    this.ca = fs.readFileSync(`${this.config['tls.ca.file']}`);

    // Back pressure
    this.messageQueue = null;
    this.currentMessageQueueLength = 0;
    this.parallelHandlers = this.config['backpressure.handlers'];
    this.maxQueueLength = this.config['backpressure.queue.length.max'];

    // Agent messenger
    this.agentMessenger = agentMessenger;

    this.logger = new Logger('MQTTClient');
  }

  /**
   * Initializes the MQTTClient loading its attributes, registering its callbacks and connecting
   * to a MQTT broker.
   *
   * @function init
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

    // Creates an async queue
    this.messageQueue = async.queue((data, done) => {
      this.asyncQueueWorker(data);
      done();
    }, this.parallelHandlers);

    // When the processing finishes, reconnects to the broker
    this.messageQueue.drain(() => {
      if (this.isConnected === false) {
        this.mqttc.reconnect();
      }
    });
  }

  /**
   * Reached when the MQTTClient connects successfully to the MQTT broker.
   *
   * @callback MqttClient~onConnect
   */
  onConnect() {
    this.isConnected = true;
    this.logger.info(`Client ${this.clientId} connected successfully!`);
    this.subscribe();
  }

  /**
   * Reached when the MQTTClient disconnects from the broker.
   *
   * @callback MqttClient~onDisconnect
   */
  onDisconnect() {
    this.logger.info(`Client ${this.clientId} disconnected, reconnecting ......`);
    this.isConnected = false;
    this.mqttc.reconnect();
  }

  /**
   * Reached when a message arrives on the topic.
   *
   * @callback MqttClient~onMessage
   *
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
   * Connects the MQTTClient to a MQTT broker.
   *
   * @function connect
   */
  connect() {
    if (this.isConnected === false) {
      this.logger.info(`Connecting to broker ${this.host}:${this.port} with protocol ${this.secureMode ? 'MQTTS' : 'MQTT'}`);
      this.mqttc = mqtt.connect(this.mqttOptions);
    }
  }

  /**
   * Subscribes the client to its topics.
   *
   * @function subscribe
   */
  subscribe() {
    this.logger.info(`Subscribing to topic ${this.config['client.subscription.topic']}`);
    if (this.isConnected === true) {
      this.mqttc.subscribe(this.config['client.subscription.topic'], { qos: this.config['client.subscription.qos'] });
    }
  }

  /**
   * Worker that sends the message.
   *
   * @private
   * @function asyncQueueWorker
   *
   * @param {Object} data
   */
  asyncQueueWorker(data) {
    const { topic, message } = data;
    this.agentMessenger.sendMessage(topic, message);
  }
}

module.exports = MQTTClient;
