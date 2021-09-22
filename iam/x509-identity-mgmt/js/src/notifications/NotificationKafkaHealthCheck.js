/**
 * Notification Kafka Producer Health Check
 */
class NotificationKafkaHealthCheck {
  /**
   * The dependencies are injected through the constructor
   */
  constructor({
    notificationKafkaProducer,
    logger,
  }) {
    Object.defineProperty(this, 'kafkaProducer', { value: notificationKafkaProducer });
    Object.defineProperty(this, 'logger', { value: logger });
  }

  /**
   * Checks the readiness of the component and reports it to the ServiceStateManager.
   *
   * @param {function} signalReady
   * @param {function} signalNotReady
   */
  async readiness(signalReady, signalNotReady) {
    try {
      const status = await this.kafkaProducer.getStatus();
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

module.exports = NotificationKafkaHealthCheck;
