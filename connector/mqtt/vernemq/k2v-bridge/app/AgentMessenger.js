const { logger } = require('@dojot/dojot-module-logger');
const { IoTAgent } = require('@dojot/iotagent-nodejs');
const defaultConfig = require('./config');
const ProjectUtils = require('./utils/utils');

const TAG = { filename: 'AgentMessenger' };

/**
 * Class representing an AgentMessenger
 * @class
 */
class AgentMessenger {
  /**
   *
   * @param {Object} mqttClient - an MQTTClient to publish consumed messages.
   * @param {object} config - the agent configurations
   */
  constructor(mqttClient, config) {
    this.config = config || defaultConfig;
    this.mqttClient = mqttClient;
    this.iotagent = null;
  }

  /**
   * @function init
   * Initializes the agent messenger and register callbacks for incoming messages,
   * calling the mqttClient to publish the messages
   */
  init() {
    this.iotagent = new IoTAgent();
    this.iotagent.init().then(() => {
      logger.info('IOT Agent initialized successfully', TAG);

      /* Actuation message handler */
      this.iotagent.on('iotagent.device', 'device.configure', (tenant, event) => {
        logger.debug(`Got device actuation message. Tenant is ${tenant}.`, TAG);

        /* generate topic - dojot's style */
        const configTopic = ProjectUtils.generateDojotActuationTopic(event.meta.service,
          event.data.id, this.config.mqtt.publishTopicSuffix);

        this.mqttClient.publishMessage(configTopic, JSON.stringify(event.data.attrs));
      });
    }).catch(() => {
      logger.error('An error occurred while initializing the IoTAgent. Bailing out!', TAG);
    });
  }
}

module.exports = AgentMessenger;
