const { ConfigManager } = require('@dojot/microservice-sdk');

const fs = require('fs');
const async = require('async');
const mqtt = require('mqtt');
const camelCase = require('lodash.camelcase');

const Utils = require('./Utils');

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
  constructor(agentMessenger, serviceStateManager, logger, deviceManagerService) {
    if (!agentMessenger) {
      throw new Error('no AgentMessenger instance was passed');
    }
    if (!serviceStateManager) {
      throw new Error('no ServiceStateMessenger instance was passed');
    }

    this.agentMessenger = agentMessenger;
    this.serviceStateManager = serviceStateManager;
    this.logger = logger;
    this.deviceManagerService = deviceManagerService;
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
    this.mqttClient.on('reconnect', this.onReconnect.bind(this));
    this.mqttClient.on('close', this.onClose.bind(this));
    this.mqttClient.on('error', this.onError.bind(this));
    this.mqttClient.on('message', this.onMessage.bind(this));

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
   *
   * @function connectOrDisconnectClient
   * @param {boolean} connect - indicate wheter to connect or disconnect the client
   */
  connectOrDisconnectClient(connect) {
    if (connect) {
      if (!this.isConnected) {
        this.mqttClient.reconnect();
      }
      return;
    }
    this.mqttClient.end(true);
  }

  /**
   * Called whenever a reconnect call has been made.
   *
   * @function onReconnect
   * @private
   */
  onReconnect() {
    this.logger.info('Trying to reconnect to the broker...');
  }

  /**
   * Reached when the MQTTClient disconnects from the broker.
   *
   * @function onClose
   * @private
   */
  onClose() {
    this.logger.warn(`Client ${this.mqttOptions.clientId} disconnected`);
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
    // Since we are reusing the sessions, we need to unsubscribe to the topic to be able to
    // correctly subscribe to it again when the client reconnects to the broker
    this.mqttClient.unsubscribe(this.config.subscription.topic);
    this.mqttClient.subscribe(
      this.config.subscription.topic,
      { qos: this.config.subscription.qos },
      (error, granted) => {
        if (granted.length > 0) {
          this.logger.info('... successfully subscribed to the topic');
          this.serviceStateManager.signalReady(this.stateService);
        } else {
          this.logger.error('Could not subscribe to the topic. Bailing out!');
          process.exit(1);
        }
      },
    );
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
    this.isDeviceDisabled(topic, message).then((res) => {
      if (!res) {
        this.agentMessenger.sendMessage(topic, message);
      }
    }).catch((error) => {
      const jsonPayload = JSON.parse(message);
      const deviceDataMessage = Utils.generateDojotDeviceDataMessage(topic, jsonPayload);
      this.logger.warn(`Error when trying to get device information ${deviceDataMessage.metadata.deviceid}`);
      if (error) {
        this.logger.error(error.stack || error);
      }
    });
  }

  async isDeviceDisabled(topic, message) {
    const jsonPayload = JSON.parse(message);
    const deviceDataMessage = Utils.generateDojotDeviceDataMessage(topic, jsonPayload);
    const deviceInformation = await this.deviceManagerService.getDevice(
      deviceDataMessage.metadata.tenant,
      deviceDataMessage.metadata.deviceid,
    );
    if (deviceInformation.disabled) {
      const error = `Device ${deviceInformation.label} is disabled. The message will be discarded.`;
      this.logger.warn(error);
      return true;
    }
    return false;
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
