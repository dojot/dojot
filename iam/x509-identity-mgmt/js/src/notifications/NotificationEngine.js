class NotificationEngine {
  constructor({
    notificationKafkaProducer, logger, service, contentType,
  }) {
    Object.defineProperty(
      this, 'producer', { value: notificationKafkaProducer },
    );
    Object.defineProperty(
      this, 'logger', { value: logger },
    );
    Object.defineProperty(
      this, 'service', { value: service },
    );
    Object.defineProperty(
      this, 'contentType', { value: contentType },
    );
  }

  /**
   * initializes the Notification Engine
   */
  async start() {
    await this.producer.connect();
  }

  /**
   * stops the Notification Engine
   */
  async stop() {
    await this.producer.disconnect();
  }

  async notify({
    tenant, topic, eventType, eventData, partitionKey, xRequestId,
  }) {
    const message = this.generateMessage(
      tenant, eventType, eventData, xRequestId,
    );
    const msgStr = JSON.stringify(message);
    await this.producer.produce(
      topic, msgStr, partitionKey,
    );
  }

  generateMessage(
    tenant, eventType, eventData, xRequestId,
  ) {
    return {
      metadata: this.generateMetadata(tenant, xRequestId),
      data: {
        eventType,
        eventData,
      },
    };
  }

  generateMetadata(tenant, xRequestId) {
    return {
      msgid: xRequestId,
      timestamp: Date.now(),
      service: this.service,
      tenant,
      contentType: this.contentType,
    };
  }
}

module.exports = NotificationEngine;
