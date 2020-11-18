const { ServiceStateManager } = require('@dojot/microservice-sdk');

class StateManager {
  constructor() {
    this.serviceStateManager = new ServiceStateManager();
  }
}

module.exports = new StateManager().serviceStateManager;
