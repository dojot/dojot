const { v4: uuidv4 } = require('uuid');

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
    tenant, topic, eventType, eventData, partitionKey,
  }) {
    const message = this.generateMessage(
      tenant, eventType, eventData,
    );
    const msgStr = JSON.stringify(message);
    await this.producer.produce(
      topic, msgStr, partitionKey,
    );
  }

  generateMessage(
    tenant, eventType, eventData,
  ) {
    return {
      metadata: this.generateMetadata(tenant),
      data: {
        eventType,
        eventData,
      },
    };
  }

  generateMetadata(tenant) {
    return {
      msgid: uuidv4(),
      timestamp: Date.now(),
      service: this.service,
      tenant,
      contentType: this.contentType,
    };
  }
}

module.exports = NotificationEngine;
