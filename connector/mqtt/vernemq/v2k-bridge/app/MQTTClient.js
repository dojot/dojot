const { ConfigManager, Logger } = require('@dojot/microservice-sdk');

const fs = require('fs');
const async = require('async');
const mqtt = require('mqtt');
const camelCase = require('lodash.camelcase');

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
   */
  constructor(agentMessenger) {
    if (!agentMessenger) {
      throw new Error('no agent messenger was passed');
    }

    this.logger = new Logger('MQTTClient');

    this.mqttc = undefined;
    this.agentMessenger = agentMessenger;

    this.config = ConfigManager.getConfig('V2K');
    this.isConnected = false;

    const certificates = {
      ca: fs.readFileSync(`${this.config.mqtt.ca}`),
      key: fs.readFileSync(`${this.config.mqtt.key}`),
      cert: fs.readFileSync(`${this.config.mqtt.cert}`),
    };
    /**
     * The certificates parameters received from the ConfigManager are the location of the files.
     * Since the whole `mqtt` scope object will be passed to the MQTT client, we need to overwrite
     * the certificates parameters with their files' contents.
     */
    this.mqttOptions = { ...this.config.mqtt, ...certificates };
    // We need to transform to camelCase, since all the MQTT client options are in this format
    this.mqttOptions = ConfigManager.transformObjectKeys(this.mqttOptions, camelCase);

    // Back pressure
    this.messageQueue = null;
    this.currentMessageQueueLength = 0;
  }

  /**
   * Initializes the MQTTClient loading its attributes, registering its callbacks and connecting
   * to a MQTT broker.
   *
   * @function init
   */
  init() {
    this.logger.info('Initializing the MQTT client...');
    this.connect();
    this.mqttc.on('connect', this.onConnect.bind(this));
    this.mqttc.on('disconnect', this.onDisconnect.bind(this));
    this.mqttc.on('error', this.onError.bind(this));
    this.mqttc.on('message', this.onMessage.bind(this));

    // Creates an async queue
    this.messageQueue = async.queue((data, done) => {
      this.asyncQueueWorker(data);
      done();
    }, this.config.backpressure.handlers);

    // When the processing finishes, reconnects to the broker
    this.messageQueue.drain(() => {
      if (this.isConnected === false) {
        this.mqttc.reconnect();
      }
    });

    this.logger.info('... successfully initialized the MQTT client');
  }

  /**
   * Reached when the MQTTClient connects successfully to the MQTT broker.
   *
   * @callback MQTTClient~onConnect
   */
  onConnect() {
    this.logger.info(`Client ${this.mqttOptions.clientId} connected successfully!`);
    this.isConnected = true;
    this.subscribe();
  }

  /**
   * Reached when the MQTTClient disconnects from the broker.
   *
   * @callback MQTTClient~onDisconnect
   */
  onDisconnect() {
    this.logger.info(`Client ${this.mqttOptions.clientId} disconnected, reconnecting...`);
    this.isConnected = false;
    this.mqttc.reconnect();
  }

  /**
   * Handles MQTT errors.
   *
   * @param {*} error
   */
  onError(error) {
    this.logger.error('An error has occurred in the MQTT connection.');
    if (error) {
      this.logger.error(error.stack || error);
    }
    this.logger.error('Bailing out!');
    process.exit(1);
  }

  /**
   * Reached when a message arrives on the topic.
   *
   * @callback MQTTClient~onMessage
   *
   * @param {string} topic
   * @param {Object} message
   * @param {Object} packet
   */
  onMessage(topic, message, packet) {
    // pause
    if (this.isConnected) {
      const size = message.toString().length;
      // check if message is duplicated
      if (packet.dup === false) {
        this.currentMessageQueueLength += size;
        const data = { topic, message };
        this.messageQueue.push(data, () => {
          this.currentMessageQueueLength -= size;
        });
      }
    }

    if (this.currentMessageQueueLength > this.config.backpressure['queue.length.max']) {
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
      this.logger.info(
        `Connecting to broker ${this.mqttOptions.host}:${this.mqttOptions.port} with ${this.mqttOptions.protocol.toUpperCase()} protocol`,
      );
      this.mqttc = mqtt.connect(this.mqttOptions);
    }
  }

  /**
   * Subscribes the client to its topics.
   *
   * @function subscribe
   */
  subscribe() {
    this.logger.info(`Subscribing to the topic ${this.config.subscription.topic}`);
    if (this.isConnected === true) {
      this.mqttc.subscribe(this.config.subscription.topic, { qos: this.config.subscription.qos });
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
