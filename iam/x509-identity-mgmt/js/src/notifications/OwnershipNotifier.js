const CREATE_EVENT = 'ownership.create';
const UPDATE_EVENT = 'ownership.update';
const DELETE_EVENT = 'ownership.delete';

function belongsToWhom(owner) {
  let ownerIdentifier = null;
  const belongsTo = {};

  if (!owner) {
    return null;
  }

  // checks who the certificate belongs to...
  if (typeof owner.device === 'string') {
    // the certificate belongs to a device!
    belongsTo.device = owner.device;
    ownerIdentifier = belongsTo.device;
  } else if (typeof owner.application === 'string') {
    // the certificate belongs to an application!
    belongsTo.application = owner.application;
    ownerIdentifier = belongsTo.application;
  } else {
    return null;
  }
  return { ownerIdentifier, belongsTo };
}

/**
 * Responsible for notifying events related to the ownership of a certificate
 */
class OwnershipNotifier {
  constructor({
    tenant = '', kafkaTopicSuffix, notificationEngine, logger,
  }) {
    Object.defineProperty(this, 'tenant', { value: tenant });
    Object.defineProperty(this, 'topic', { value: (tenant) ? `${tenant}.${kafkaTopicSuffix}` : `${kafkaTopicSuffix}` });
    Object.defineProperty(this, 'notificationEngine', { value: notificationEngine });
    Object.defineProperty(this, 'logger', { value: logger });
  }

  /**
   * Notifies the creation of ownership (when an owner is defined for a certificate).
   *
   * Event (to Kafka) issued when an owner is associated with the certificate
   * (created a "declaration of ownership" involving the certificate and the owner).
   *
   * @param {CertificateModel} certRecord representing the certificate, modeled using Mongoose.
   * @param {object} owner to which the certificate now belongs.
   */
  async creation(certRecord, owner = {}) {
    const eventType = CREATE_EVENT;

    // checks who the certificate belongs to...
    const toWhom = belongsToWhom(owner);
    if (!toWhom) {
      this.logger.warn(`No '${eventType}' notification will be issued. `
        + `The certificate '${certRecord.fingerprint}' does not belong to anyone.`);
      return;
    }
    const { ownerIdentifier, belongsTo } = toWhom;

    // Data to be published on Kafka
    const eventData = {
      fingerprint: certRecord.fingerprint,
      certificatePem: certRecord.pem,
      issuedByDojotPki: certRecord.issuedByDojotPki,
      autoRegistered: certRecord.autoRegistered,
      belongsTo,
    };

    // Delegates to the notification engine to publish data on Kafka
    await this.notificationEngine.notify({
      tenant: this.tenant,
      topic: this.topic,
      eventType,
      eventData,
      partitionKey: `${this.tenant}:${ownerIdentifier}`,
    });

    const ownerKey = `${this.tenant}:${ownerIdentifier}`;

    this.logger.info(`Notification issued: '${eventType}'. `
      + `Certificate '${certRecord.fingerprint}'. `
      + `Owner '${ownerKey}'.`);
  }

  /**
    * Notifies the change in ownership of a certificate, as long as it belongs to an owner.
    *
    * Event (to Kafka) issued when the "declaration of ownership"
    * of the certificate is transferred from one owner to another.
    *
    * @param {CertificateModel} certRecord representing the certificate, modeled using Mongoose.
    * @param {object} previousOwner to which the certificate belonged before.
    * @param {object} currentOwner to which the certificate now belongs.
    */
  async change(certRecord, previousOwner = {}, currentOwner = {}) {
    const eventType = UPDATE_EVENT;

    // checks who the certificate belonged to before...
    const previousToWhom = belongsToWhom(previousOwner);
    if (!previousToWhom) {
      this.logger.warn(`No '${eventType}' notification will be issued. `
        + `The certificate '${certRecord.fingerprint}' did not belong to anyone.`);
      return;
    }
    const {
      ownerIdentifier: previousOwnerIdentifier,
      belongsTo: previousBelongsTo,
    } = previousToWhom;

    // checks who the certificate belongs to now...
    const toWhom = belongsToWhom(currentOwner);
    if (!toWhom) {
      this.logger.warn(`No '${eventType}' notification will be issued. `
        + `The certificate '${certRecord.fingerprint}' does not belong to anyone.`);
      return;
    }
    const { ownerIdentifier, belongsTo } = toWhom;

    // Data to be published on Kafka
    const eventData = {
      fingerprint: certRecord.fingerprint,
      certificatePem: certRecord.pem,
      issuedByDojotPki: certRecord.issuedByDojotPki,
      autoRegistered: certRecord.autoRegistered,
      previousBelongsTo,
      belongsTo,
    };

    // Delegates to the notification engine to publish data on Kafka
    await this.notificationEngine.notify({
      tenant: this.tenant,
      topic: this.topic,
      eventType,
      eventData,
      partitionKey: `${this.tenant}:${ownerIdentifier}`,
    });

    const prevOwnerKey = `${this.tenant}:${previousOwnerIdentifier}`;
    const currOwnerKey = `${this.tenant}:${ownerIdentifier}`;

    this.logger.info(`Notification issued: '${eventType}'. `
      + `Certificate '${certRecord.fingerprint}'. `
      + `From previous '${prevOwnerKey}' to current '${currOwnerKey}' owner.`);
  }

  /**
   * Notifies the removal of ownership of a certificate. For that, it is
   * necessary that the certificate has already belonged to an owner before.
   *
   * Event (to Kafka) issued when the "declaration of ownership" is removed,
   * or when the certificate is removed, as long as it belongs to an owner.
   *
   * @param {CertificateModel} certRecord representing the certificate, modeled using Mongoose.
   * @param {object} previousOwner to which the certificate belonged before.
   */
  async removal(certRecord, previousOwner = {}) {
    const eventType = DELETE_EVENT;

    // checks who the certificate belonged to before...
    const previousToWhom = belongsToWhom(previousOwner);
    if (!previousToWhom) {
      this.logger.warn(`No '${eventType}' notification will be issued. `
        + `The certificate '${certRecord.fingerprint}' did not belong to anyone.`);
      return;
    }
    const {
      ownerIdentifier: previousOwnerIdentifier,
      belongsTo: previousBelongsTo,
    } = previousToWhom;

    // Data to be published on Kafka
    const eventData = {
      fingerprint: certRecord.fingerprint,
      certificatePem: certRecord.pem,
      issuedByDojotPki: certRecord.issuedByDojotPki,
      autoRegistered: certRecord.autoRegistered,
      previousBelongsTo,
    };

    // Delegates to the notification engine to publish data on Kafka
    await this.notificationEngine.notify({
      tenant: this.tenant,
      topic: this.topic,
      eventType,
      eventData,
      partitionKey: `${this.tenant}:${previousOwnerIdentifier}`,
    });

    const prevOwnerKey = `${this.tenant}:${previousOwnerIdentifier}`;
    this.logger.info(`Notification issued: '${eventType}'. `
      + `Certificate '${certRecord.fingerprint}'. `
      + `Owner '${prevOwnerKey}'.`);
  }
}

module.exports = OwnershipNotifier;
