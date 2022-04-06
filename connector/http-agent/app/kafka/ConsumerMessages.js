const {
  Kafka: { Consumer },
} = require('@dojot/microservice-sdk');

/**
 * This class handles messages from dojot topics on kafka
 * @class
 */
class KafkaConsumer extends Consumer {
  /**
   * Create an instance
   */
  constructor(tenantService, config, logger) {
    logger.debug('constructor: Instantiating a Kafka Consumer');

    super({
      ...config.sdk,
      'enable.async.commit': true,
      'kafka.consumer': config.consumer,
      'kafka.topic': config.topic,
    });

    this.config = config;
    this.idCallbackTenant = null;
    this.logger = logger;
    this.tenantService = tenantService;
  }

  init() {
    super.init();
    this.initCallbackForNewTenantEvents();
  }

  getCallbackForNewTenantEvents() {
    const operations = {
      CREATE: this.tenantService.create.bind(this.tenantService),
      DELETE: this.tenantService.remove.bind(this.tenantService),
    };

    return (data, ack) => {
      try {
        const { value: payload } = data;
        const payloadObject = JSON.parse(payload.toString());

        this.logger.info('New tenant event received');
        operations[payloadObject.type]({
          id: payloadObject.tenant,
          signatureKey: payloadObject.signatureKey,
        }).then(() => {
          ack();
        }).catch((error) => {
          this.logger.error(`Dispatch failed. ${error.message}`);
          ack();
        });
      } catch (error) {
        this.logger.error(error);
        ack();
      }
    };
  }

  /*
  * Initializes the consumption of the tenancy topic
  *
  * @public
  */
  initCallbackForNewTenantEvents() {
    this.logger.debug(`initCallbackForTenantEvents: Register Callbacks for
      topics with regex ${this.config.subscribe['topics.regex.tenants']}`);
    // eslint-disable-next-line security/detect-non-literal-regexp
    const topic = new RegExp(this.config.subscribe['topics.regex.tenants']);

    this.idCallbackTenant = this.registerCallback(topic, this.getCallbackForNewTenantEvents());
    this.logger.debug('registerCallback: Registered Callback');
  }


  /**
   * A function to get if kafka is connected
   *
   * @returns {Promise<boolean>} if kafka is connect
   */
  async isConnected() {
    try {
      const { connected } = await this.getStatus();
      if (connected) {
        return true;
      }
      return false;
    } catch (e) {
      this.logger.error('isConnected:', e);
      return false;
    }
  }

  /**
   * Unregister all callbacks
   *
   * @throws If Cannot unregister callback
   */
  unregisterCallbacks() {
    if (this.idCallbackTenant) {
      this.unregisterCallback(this.idCallbackTenant);
      this.idCallbackTenant = null;
      this.logger.debug('unregisterCallbacks: Unregistered callback for tenant');
    } else {
      this.logger.warn('unregisterCallbacks: Doesn\'t exist Callback to unregister for tenant');
    }
  }
}

module.exports = KafkaConsumer;
