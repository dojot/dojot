const {
  ServiceStateManager: { Manager },
} = require('@dojot/microservice-sdk');


/**
 * This class has responsible for the health check and graceful shutdown
 */
class ServiceStateMgmt {
  /**
     *
     */
  constructor() {
    this.servername = 'influxdb-storer';
    this.manager = new Manager([this.servername]);
  }

  /**
     * Adds a new health checker function.
     *
     * @param {(Function, Function) => void} func function to be registered as a health checker. It
     * should receive two functions as parameters: `signalReady` and `signalNotReady`. Use them to
     * submit the state of your service.
     * @param {number} interval period of time (in ms) to execute the health checker.
     *
     */
  addHealthChecker(func, interval) {
    setInterval(
      () => {
        func(this.signalReady.bind(this), this.signalNotReady.bind(this));
      },
      interval,
    );
    // TODO clear interval
  }

  /**
     * This register what the Shutdown should do when call this.shutdown()
     *
     * @param {async () => void} func Async function that are called when shutdown is initialized
     */
  registerShutdown(func) {
    this.manager.registerShutdownHandler(func);
  }

  /**
     *  Signals to the health check service that the service is ready.
     */
  signalReady() {
    this.manager.signalReady(this.servername);
  }

  /**
     *  Signals to the health check service that the service is not ready.
     */
  signalNotReady() {
    this.manager.signalNotReady(this.servername);
  }

  /**
   * Performer a graceful Shutdown
   */
  async shutdown() {
    await this.manager.shutdown();
  }
}

module.exports = new ServiceStateMgmt();
