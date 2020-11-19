const { ServiceStateManager } = require('@dojot/microservice-sdk');
const { getConfig, transformObjectKeys } = require('@dojot/microservice-sdk/lib/configManager');
const { camelCase } = require('lodash');

class StateManager {
  constructor() {
    const { lightship } = getConfig('KAFKA_WS');
    const serviceConf = { lightship: transformObjectKeys(lightship, camelCase) };
    this.serviceStateManager = new ServiceStateManager(serviceConf);
  }
}

module.exports = new StateManager().serviceStateManager;
