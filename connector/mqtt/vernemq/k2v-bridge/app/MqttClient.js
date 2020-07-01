const { Logger } = require('@dojot/microservice-sdk');

const fs = require('fs');
const util = require('util');
const mqtt = require('mqtt');

const { mqtt: mqttConfig } = require('./config');
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

    this.clientId = mqttConfig['client.id'];
    this.keepalive = mqttConfig['client.keepalive'];
    this.publishQos = mqttConfig['client.publish.qos'];
    this.secureMode = mqttConfig['client.secure'];
    this.username = mqttConfig['client.username'];

    this.host = mqttConfig['server.address'];
    this.port = mqttConfig['server.port'];

    this.privateKey = fs.readFileSync(`${mqttConfig['tls.key.file']}`);
    this.clientCrt = fs.readFileSync(`${mqttConfig['tls.certificate.file']}`);
    this.ca = fs.readFileSync(`${mqttConfig['tls.ca.file']}`);

    this.mqttc = null;
    this.mqttOptions = null;
    this.agentMessenger = null;

    this.logger = new Logger('MQTTClient');
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

    this.mqttOptions = {
      username: this.username,
      clientId: this.clientId,
      host: this.host,
      port: this.port,
      protocol: this.secureMode ? 'mqtts' : 'mqtt',
      ca: this.ca,
      key: this.privateKey,
      cert: this.clientCrt,
      keepAlive: this.keepalive,
      clean: false,
      rejectUnauthorized: true,
    };

    this.mqttc = mqtt.connect(this.mqttOptions);

    this.logger.info('Binding event callbacks...');
    this.mqttc.on('connect', this.onConnect.bind(this));
    this.mqttc.on('disconnect', this.onDisconnect.bind(this));
    this.mqttc.on('error', this.onError.bind(this));
    this.logger.info('... Binded event callbacks');
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
   * Reached when the MQTTClient disconnects from the broker.
   * @access private
   * @callback MQTTClient~onDisconnect
   */
  onDisconnect() {
    this.isConnected = false;
    this.logger.info('MQTT connection ended, trying to reconnect...');
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
      if (this.isConnected && this.mqttc) {
        const value = JSON.parse(data.value.toString());

        const topic = utils.generateDojotActuationTopic(
          value.meta.service,
          value.data.id,
          mqttConfig['client.publish.topic.suffix'],
        );

        this.mqttc.publish(
          topic,
          JSON.stringify(value.data.attrs),
          { qos: this.publishQos },
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
