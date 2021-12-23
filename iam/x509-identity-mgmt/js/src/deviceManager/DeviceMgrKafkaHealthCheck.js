/**
 * Device Manager KafkaConsumer Health Check
 */
class DeviceMgrKafkaHealthCheck {
  /**
   * The dependencies are injected through the constructor
   */
  constructor({
    deviceMgrKafkaConsumer,
    logger,
  }) {
    Object.defineProperty(
      this, 'kafkaConsumer', { value: deviceMgrKafkaConsumer },
    );
    Object.defineProperty(
      this, 'logger', { value: logger },
    );
  }

  /**
   * Checks the readiness of the component and reports it to the ServiceStateManager.
   *
   * @param {function} signalReady
   * @param {function} signalNotReady
   */
  async readiness(signalReady, signalNotReady) {
    try {
      const status = await this.kafkaConsumer.getStatus();
      if (status.connected) {
        signalReady();
      } else {
        signalNotReady();
      }
    } catch (ex) {
      this.logger.error(ex);
      signalNotReady();
    }
  }
}

module.exports = DeviceMgrKafkaHealthCheck;
