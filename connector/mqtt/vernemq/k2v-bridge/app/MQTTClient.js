const {
  ConfigManager: { getConfig, transformObjectKeys },
  Logger,
} = require('@dojot/microservice-sdk');

const camelCase = require('lodash.camelcase');
const fs = require('fs');
const util = require('util');
const mqtt = require('mqtt');

const Utils = require('./Utils');

// PR instrução


/**
 * Class representing a MQTTClient
 * @class
 */
class MQTTClient {
  /**
   * Creates a MQTTClient
   *
   * @param {Object} agentMessenger
   * @param {Object} serviceStateManager
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
    this.logger = new Logger('k2v:mqtt-client');

    this.agentMessenger = agentMessenger;

    this.serviceStateManager = serviceStateManager;

    this.mqttClient = undefined;
    this.isConnected = false;

    const config = getConfig('K2V');

    this.publishConfig = config.publish;

    const certificates = {
      ca: fs.readFileSync(`${config.mqtt.ca}`),
      key: fs.readFileSync(`${config.mqtt.key}`),
      cert: fs.readFileSync(`${config.mqtt.cert}`),
    };
    /**
     * The certificates parameters received from the ConfigManager are the location of the files.
     * Since the whole `mqtt` scope object will be passed to the MQTT client, we need to overwrite
     * the certificates parameters with their files' contents.
     */
    this.mqttOptions = transformObjectKeys(
      { ...config.mqtt, ...certificates },
      camelCase,
    );
  }

  /**
   * Initializes the mqttClient loading it's attributes, registering it's callbacks and connecting
   * to a broker.
   *
   * @function init
   * @public
   */
  init() {
    this.logger.info('Connecting MQTT client...');

    this.mqttClient = mqtt.connect(this.mqttOptions);

    this.logger.info('Binding event callbacks...');
    this.mqttClient.on('connect', this.onConnect.bind(this));
    this.mqttClient.on('reconnect', this.onReconnect.bind(this));
    this.mqttClient.on('close', this.onClose.bind(this));
    this.logger.info('... bound event callbacks');
  }

  /**
   * Reached when the MQTTClient connects successfully to the broker
   *
   * @function onConnect
   * @private
   */
  async onConnect() {
    if (!this.isConnected) {
      await this.agentMessenger.init(this);
      this.isConnected = true;
      this.logger.info('MQTT connection established');
      this.serviceStateManager.signalReady('mqtt');
    }
  }

  onReconnect() {
    this.logger.warn('Trying to reconnect to the broker...');
  }

  /**
   * Reached when the MQTT connection from the broker is closed.
   *
   * @function onClose
   * @private
   */
  async onClose() {
    await this.agentMessenger.finish();
    this.isConnected = false;
    this.serviceStateManager.signalNotReady('mqtt');
    this.logger.warn('MQTT connection closed');
  }

  /**
   * Publishes a message to a given topic.
   *
   * @param {*} data message object retrieved from Kafka
   *
   * @function publishMessage
   * @public
   */
  publishMessage(data) {
    try {
      if (this.isConnected && this.mqttClient) {
        const value = JSON.parse(data.value.toString());

        const topic = Utils.generateDojotActuationTopic(
          value.meta.service,
          value.data.id,
          this.publishConfig['topic.suffix'],
        );

        this.mqttClient.publish(
          topic,
          JSON.stringify(value.data.attrs),
          { qos: this.publishConfig.qos },
          (error, packet) => {
            if (error) {
              this.logger.error(error.stack || error);
              if (packet) {
                this.logger.error(`Packet that caused the error: ${util.inspect(packet)}`);
              }
            }
          },
        );
      } else {
        this.logger.error('Client not connected');
      }
    } catch (error) {
      this.logger.error(error.stack || error);
    }
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
      this.mqttClient.end(() => {
        this.logger.warn('MQTT connection was closed!');
        return resolve();
      });
    });
  }
}

module.exports = MQTTClient;
