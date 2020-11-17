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
   * @param {Object} agentMessenger - AgentMessenger instance
   * @param {Object} serviceStateManager - ServiceStateManager instance
   *
   * @constructor
   */
  constructor(agentMessenger, serviceStateManager) {
    if (!agentMessenger) {
      throw new Error('no AgentMessenger instance was passed');
    }
    if (!serviceStateManager) {
      throw new Error('no ServiceStateMessenger instance was passed');
    }
    this.logger = new Logger('v2k:mqtt-client');

    this.agentMessenger = agentMessenger;
    this.serviceStateManager = serviceStateManager;
    /**
     * The service to be registered in the ServiceStateManager
     * @type {string}
     */
    this.stateService = 'mqtt';
    this.serviceStateManager.registerService(this.stateService);

    this.mqttClient = undefined;
    this.isConnected = false;

    this.config = ConfigManager.getConfig('V2K');
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
   * @public
   */
  init() {
    this.logger.info('Initializing the MQTT client...');

    this.connect();

    this.mqttClient.on('connect', this.onConnect.bind(this));
    this.mqttClient.on('disconnect', this.onDisconnect.bind(this));
    this.mqttClient.on('error', this.onError.bind(this));
    this.mqttClient.on('message', this.onMessage.bind(this));
    this.mqttClient.on('packetreceive', this.onPacketReceive.bind(this));

    // Creates an async queue
    this.messageQueue = async.queue((data, done) => {
      this.asyncQueueWorker(data);
      done();
    }, this.config.backpressure.handlers);

    // When the processing finishes, reconnects to the broker
    this.messageQueue.drain(() => {
      if (this.isConnected === false) {
        this.mqttClient.reconnect();
      }
    });

    this.logger.info('... successfully initialized the MQTT client');
  }

  /**
   * Reached when the MQTTClient connects successfully to the MQTT broker.
   *
   * @function onConnect
   * @private
   */
  onConnect() {
    this.logger.info(`Client ${this.mqttOptions.clientId} connected successfully!`);
    this.isConnected = true;
    this.subscribe();
  }

  /**
   * Reached when the MQTTClient disconnects from the broker.
   *
   * @function onDisconnect
   * @private
   */
  onDisconnect() {
    this.logger.warn(`Client ${this.mqttOptions.clientId} disconnected, reconnecting...`);
    this.isConnected = false;
    this.serviceStateManager.signalNotReady(this.stateService);
  }

  /**
   * Handles MQTT errors.
   *
   * @param {*} error
   *
   * @function onError
   * @private
   */
  onError(error) {
    this.logger.error(error.stack || error);
    this.isConnected = false;
    this.serviceStateManager.signalNotReady(this.stateService);
  }

  /**
   * Reached when a message arrives on the topic.
   *
   * @param {string} topic
   * @param {Object} message
   * @param {Object} packet
   *
   * @function onMessage
   * @private
   */
  onMessage(topic, message, packet) {
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
      this.mqttClient.end(true);
      this.isConnected = false;
    }
  }

  /**
   * Handles the packets that are received by the broker.
   *
   * @param {mqtt.Packet} packet
   *
   * @function onPacketReceive
   * @private
   */
  onPacketReceive(packet) {
    switch (packet.cmd) {
      // The MQTT library does not provide a onSubscribe callback, so we need to retrieve its packet
      // to properly set the service state
      case 'suback':
        this.logger.info('... successfully subscribed to the topic');
        this.serviceStateManager.signalReady(this.stateService);
        break;

      default:
        break;
    }
  }

  /**
   * Connects the MQTTClient to a MQTT broker.
   *
   * @function connect
   * @private
   */
  connect() {
    if (this.isConnected === false) {
      this.logger.info(
        `Connecting to broker ${this.mqttOptions.host}:${this.mqttOptions.port} with ${this.mqttOptions.protocol.toUpperCase()} protocol`,
      );
      this.mqttClient = mqtt.connect(this.mqttOptions);
    }
  }

  /**
   * Subscribes the client to its topics.
   *
   * @function subscribe
   * @private
   */
  subscribe() {
    this.logger.info(`Subscribing to the topic ${this.config.subscription.topic}...`);
    if (this.isConnected === true) {
      this.mqttClient.subscribe(
        this.config.subscription.topic,
        { qos: this.config.subscription.qos },
      );
    }
  }

  /**
   * Worker that sends the message.
   *
   * @param {Object} data
   *
   * @function asyncQueueWorker
   * @private
   */
  asyncQueueWorker(data) {
    const { topic, message } = data;
    this.agentMessenger.sendMessage(topic, message);
  }

  /**
   * Shutdown handler to be passed to the ServiceStateManager.
   *
   * @returns {Promise<void>}
   *
   * @function shutdownHandler
   * @public
   */
  shutdownHandler() {
    return new Promise((resolve) => {
      this.logger.warn('Closing MQTT connection...');
      this.logger.warn('Unsubscribing from topics...');
      this.mqttClient.unsubscribe(this.config.subscription.topic);
      this.mqttClient.end(() => {
        this.logger.warn('MQTT connection was closed!');
        return resolve();
      });
    });
  }
}

module.exports = MQTTClient;
