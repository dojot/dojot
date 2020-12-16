const { Kafka: { Consumer }, Logger } = require('@dojot/microservice-sdk');
const { kafka: { consumer: consumerConfig } } = require('./Config');

const logger = new Logger('kafka2ftp:app/KafkaFTPConsumers');

/**
 * Consume kafka messages to n tenants from the topic {tenant}.dojot.ftp
 */
class KafkaFTPConsumers {
  /**
   *
   */
  constructor() {
    logger.debug('constructor: Instance Kafka');
    this.consumer = new Consumer({ 'kafka.consumer': consumerConfig });
    this.registeredCallbacks = [];
  }

  /**
   * Initialize Kafka
   */
  async init() {
    try {
      logger.info('init: Kafka starting...');
      await this.consumer.init();
      logger.info('init: ...Kafka started ');
    } catch (error) {
      logger.error(`init: Error starting kafka ${error.stack}`);
      throw error;
    }
  }

  /**
   * Register Callback for a tenant in topic {tenant}.dojot.ftp
   * @param {string} tenant
   * @param {function} callback
   */
  registerCallback(tenant, callback) {
    logger.debug(`registerCallback: Register Callback for tenant ${tenant}`);
    const idCallback = this.consumer.registerCallback(`${tenant}.dojot.ftp`, callback);
    this.registeredCallbacks.push({ tenant, idCallback });
  }

  /**
   * Unregister All Callbacks
   */
  async unregisterCallbacks() {
    logger.debug('unregisterCallbacks: Unregister All Callbacks');
    await Promise.all(this.registeredCallbacks.map(async (obj) => {
      const { idCallback } = obj;
      this.consumer.unregisterCallback(idCallback);
    }));
  }
}

module.exports = KafkaFTPConsumers;
