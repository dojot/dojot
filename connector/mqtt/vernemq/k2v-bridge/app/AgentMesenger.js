const { logger } = require('@dojot/dojot-module-logger');
const { IoTAgent } = require('@dojot/iotagent-nodejs');
const defaultConfig = require('./config');
const ProjectUtils = require('./utils/utils');

const TAG = { filename: 'AgentMessenger' };

class AgentMessenger {
  constructor(mqttClient, config) {
    this.config = config || defaultConfig;
    this.mqttClient = mqttClient;
    this.iotagent = null;
  }

  init() {
    this.iotagent = new IoTAgent();
    this.iotagent.init().then(() => {
      /* Actuation message handler */
      this.iotagent.on('iotagent.device', 'device.configure', (tenant, event) => {
        logger.debug(`Got device actuation message. Tenant is ${tenant}.`, TAG);

        const configTopic = ProjectUtils.generateDojotActuationTopic(event.meta.service,
          event.data.id, this.config.mqtt.publishSuffix);

        this.mqttClient.publishMessage(configTopic, JSON.stringify(event.data.attrs));
      });
    }).catch(() => {
      logger.error('An error occurred while initializing the IoTAgent. Bailing out!', TAG);
    });
  }
}

module.exports = AgentMessenger;
