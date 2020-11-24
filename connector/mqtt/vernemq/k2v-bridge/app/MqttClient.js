const { ConfigManager, Logger } = require('@dojot/microservice-sdk');

const camelCase = require('lodash.camelcase');
const fs = require('fs');
const util = require('util');
const mqtt = require('mqtt');

const utils = require('./utils');
const AgentMessenger = require('./AgentMessenger');

/**
 * Class representing a MQTTClient
 * @class
 */
class MQTTClient {
  /**
   * Creates a MQTTClient
   * @access public
   * @constructor
   */
  constructor() {
    this.isConnected = false;

    const config = ConfigManager.getConfig('K2V');
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
    this.mqttOptions = ConfigManager.transformObjectKeys(
      { ...config.mqtt, ...certificates },
      camelCase,
    );

    this.mqttClient = null;
    this.agentMessenger = null;

    this.logger = new Logger('k2v:mqtt-client');
  }

  /**
   * Initializes the mqttClient loading it's attributes, registering it's callbacks and connecting
   * to a broker.
   * @access public
   * @function init
   */
  init() {
    this.logger.info('Connecting MQTT client...');

    this.agentMessenger = new AgentMessenger(this);

    this.mqttClient = mqtt.connect(this.mqttOptions);

    this.logger.info('Binding event callbacks...');
    this.mqttClient.on('connect', this.onConnect.bind(this));
    this.mqttClient.on('close', this.onClose.bind(this));
    this.mqttClient.on('error', this.onError.bind(this));
    this.logger.info('... bound event callbacks');
  }

  /**
   * Reached when the MQTTClient connects successfully to the broker
   * @access private
   * @callback MQTTClient~onConnect
   */
  onConnect() {
    this.logger.info('MQTT connection established');
    this.isConnected = true;
    this.agentMessenger.init();
  }

  /**
   * Reached when the MQTT connection from the broker is closed.
   * @access private
   * @callback MQTTClient~onClose
   */
  onClose() {
    this.isConnected = false;
    this.logger.info('MQTT connection close, trying to reconnect...');
    // TODO: stop Kafka message consumption
    // TODO: better disconnection handling
  }

  /**
   * Called when an error is thrown.
   * @access private
   * @callback MQTTClient~onError
   *
   * @param {*} error
   */
  onError(error) {
    this.logger.error('An error has occurred in the MQTT connection.');
    if (error) {
      this.logger.error(error.stack || error);
    }
    this.logger.error('Bailing out!');
    utils.killApplication();
  }

  /**
   * Publishes a message to a given topic.
   * @access public
   * @function publishMessage
   *
   * @param {*} data message object retrieved from Kafka
   */
  publishMessage(data) {
    try {
      if (this.isConnected && this.mqttClient) {
        const value = JSON.parse(data.value.toString());

        const topic = utils.generateDojotActuationTopic(
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
}

module.exports = MQTTClient;
