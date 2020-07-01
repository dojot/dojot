const { logger } = require('@dojot/dojot-module-logger');
const { Kafka: { ConsumerBackPressure } } = require('@dojot/microservice-sdk');
const { kafka: { consumer: consumerConfig } } = require('./Config');

const TAG = { filename: 'kafka2ftp:app/KafkaFTPConsumers' };


/**
 * Consume kafka messages to n tenants from the topic {tenant}.dojot.ftp
 */
class KafkaFTPConsumers {
  /**
   *
   */
  constructor() {
    logger.debug('constructor: Instance Kafka', TAG);
    this.consumer = new ConsumerBackPressure(consumerConfig);
    this.registeredCallbacks = [];
  }

  /**
   * Inicialze Kafka
   */
  async init() {
    try {
      logger.info('init: Kafka starting...', TAG);
      await this.consumer.init();
      logger.info('init: ...Kafka started ', TAG);
    } catch (error) {
      logger.error(`init: Error starting kafka ${error.stack}`, TAG);
      throw error;
    }
  }

  /**
   * Register Callback for a tenant in topic {tenant}.dojot.ftp
   * @param {string} tenant
   * @param {function} callback
   */
  registerCallback(tenant, callback) {
    logger.debug(`registerCallback: Register Callback for tenant ${tenant}`, TAG);
    const idCallback = this.consumer.registerCallback(`${tenant}.dojot.ftp`, callback);
    this.registeredCallbacks.push({ tenant, idCallback });
  }

  /**
   * Unregister All Callbacks
   */
  async unregisterCallbacks() {
    logger.debug('unregisterCallbacks: Unregister All Callbacks', TAG);
    await Promise.all(this.registeredCallbacks.map(async (obj) => {
      const { idCallback } = obj;
      this.consumer.unregisterCallback(idCallback);
    }));
  }
}

module.exports = KafkaFTPConsumers;
