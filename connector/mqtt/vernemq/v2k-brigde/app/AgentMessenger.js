const { IoTAgent } = require('@dojot/iotagent-nodejs');
const { logger } = require('@dojot/dojot-module-logger');
const defaultConfig = require('./config');
const Utils = require('./utils/utils');
const MQTTClient = require('./MqttClient');

const TAG = { filename: 'AgentMessenger' };

/**
 * Class representing an AgentMessenger
 * @class
 */
class AgentMessenger {
  /**
   * Create an agentMessenger
   * @param {Object} config - the configuration
   */
  constructor(config) {
    this.initialized = false;
    this.config = config || defaultConfig;
    this.iotagent = new IoTAgent();
    this.mqttClient = new MQTTClient(this, this.config);
  }

  /**
   * @function init
   * Initialize the agent messenger
   * on success init mqttClient
   * on error exit
   */
  init() {
    this.iotagent.init().then(() => {
      logger.debug('... IoT agent was initialized', TAG);

      // initializing mqtt client
      logger.debug('Initializing MQTTClient', TAG);
      this.mqttClient.init();
    }).catch(() => {
      logger.error('An error occurred while initializing the IoTAgent. Bailing out!', TAG);
      process.exit(1);
    });
  }

  /**
   * @function sendMessage
   * Produce a message a given message to a given topic
   * @param {string} topic - topic to produce
   * @param {Object} message - message to produce
   */
  sendMessage(topic, message) {
    try {
      logger.debug('Sending messaged to kafka', TAG);

      const jsonPayload = JSON.parse(message);
      const deviceDataMessage = Utils.generateDojotDeviceDataMessage(topic, jsonPayload);
      const messageKey = `${deviceDataMessage.metadata.tenant}:${deviceDataMessage.metadata.deviceid}`;
      this.iotagent.updateAttrs(deviceDataMessage.metadata.deviceid,
        deviceDataMessage.metadata.tenant,
        deviceDataMessage.attrs,
        deviceDataMessage.metadata,
        messageKey);
    } catch (error) {
      logger.error(`Failed to send message to kafka. Error: ${error}`, TAG);
    }
  }
}

module.exports = AgentMessenger;
