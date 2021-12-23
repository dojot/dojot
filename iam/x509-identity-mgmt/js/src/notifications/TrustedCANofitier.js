const CREATE_EVENT = 'trustedca.create';
const DELETE_EVENT = 'trustedca.delete';

/**
 * Responsible for notifying events related to the Trusted CA of a certificate
 */
class TrustedCANotifier {
  constructor({
    tenant = '', kafkaTopicSuffix, notificationEngine, logger,
  }) {
    Object.defineProperty(
      this, 'tenant', { value: tenant },
    );
    Object.defineProperty(
      this, 'topic', { value: (tenant) ? `${tenant}.${kafkaTopicSuffix}` : `${kafkaTopicSuffix}` },
    );
    Object.defineProperty(
      this, 'notificationEngine', { value: notificationEngine },
    );
    Object.defineProperty(
      this, 'logger', { value: logger },
    );
  }

  /**
   * Notifies the creation of a record for a trusted CA.
   *
   * Event (to Kafka) issued after creating a record for a trusted CA
   *
   * @param {TrustedCAModel} caCertRecord representing the CA certificate, modeled using Mongoose.
   */
  async creation(caCertRecord) {
    const eventType = CREATE_EVENT;

    // Data to be published on Kafka
    const eventData = {
      caFingerprint: caCertRecord.caFingerprint,
      caPem: caCertRecord.caPem,
      allowAutoRegistration: caCertRecord.allowAutoRegistration,
    };

    // Delegates to the notification engine to publish data on Kafka
    await this.notificationEngine.notify({
      tenant: this.tenant,
      topic: this.topic,
      eventType,
      eventData,
      partitionKey: `${this.tenant}:${caCertRecord.caFingerprint}`,
    });

    this.logger.info(`Notification issued: '${eventType}'. `
      + `CA Certificate '${caCertRecord.caFingerprint}'.`);
  }

  /**
   * Notifies the removal of a record to a trusted CA.
   *
   * Event (to Kafka) issued after removing a record for a trusted CA
   *
   * @param {TrustedCAModel} caCertRecord representing the CA certificate, modeled using Mongoose.
   */
  async removal(caCertRecord) {
    const eventType = DELETE_EVENT;

    // Data to be published on Kafka
    const eventData = {
      caFingerprint: caCertRecord.caFingerprint,
      caPem: caCertRecord.caPem,
      allowAutoRegistration: caCertRecord.allowAutoRegistration,
    };

    // Delegates to the notification engine to publish data on Kafka
    await this.notificationEngine.notify({
      tenant: this.tenant,
      topic: this.topic,
      eventType,
      eventData,
      partitionKey: `${this.tenant}:${caCertRecord.caFingerprint}`,
    });

    this.logger.info(`Notification issued: '${eventType}'. `
      + `CA Certificate '${caCertRecord.caFingerprint}'.`);
  }
}

module.exports = TrustedCANotifier;
