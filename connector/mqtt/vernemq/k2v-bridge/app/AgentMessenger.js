const { logger } = require('@dojot/dojot-module-logger');
const { IoTAgent } = require('@dojot/iotagent-nodejs');
const nodeUtil = require('util');
const defaultConfig = require('./config');
const utils = require('./utils/utils');

const TAG = { filename: 'AgentMessenger' };

/**
 * An agent to consume messages from pre-defined topics in Apache Kafka,
 * format them and publish the formatted messages to pre-defined topics in VerneMQ
 * @class
 */
class AgentMessenger {
  /**
   * Creates an Agent Messenger
   * @param {Object} mqttClient - a mqtt client to connect to VerneMQ broker.
   * @param {object} config - the agent configurations
   */
  constructor(mqttClient, config) {
    this.config = config || defaultConfig;
    this.mqttClient = mqttClient;
    this.iotagent = null;
  }

  /**
   * @function init
   * Initializes the agent messenger and registers callbacks for incoming messages.
   */
  init() {
    this.iotagent = new IoTAgent();
    this.iotagent.init().then(() => {
      logger.info('IOT Agent initialized successfully', TAG);

      /* Actuation message handler */
      this.iotagent.on('iotagent.device', 'device.configure', (tenant, data) => {
        logger.debug(`Got device actuation message. Tenant is ${tenant} with message ${nodeUtil.inspect(data)}.`, TAG);

        /* generate topic - dojot's style */
        const configTopic = utils.generateDojotActuationTopic(data.meta.service,
          data.data.id, this.config.mqtt.publishTopicSuffix);

        this.mqttClient.publishMessage(configTopic, JSON.stringify(data.data.attrs));
      });
    }).catch(() => {
      logger.error('An error occurred while initializing the IoTAgent. Bailing out!', TAG);
      process.exit(1);
    });
  }
}

module.exports = AgentMessenger;
