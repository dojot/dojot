const { ServiceStateManager } = require('@dojot/microservice-sdk');

const services = ['kafka', 'server', 'redis'];

class StateManager {
  constructor() {
    this.manager = new ServiceStateManager.Manager(services);
  }

  signalReady(service) {
    return this.manager.signalReady(service);
  }

  signalNotReady(service) {
    return this.manager.signalNotReady(service);
  }
}

module.exports = new StateManager();
