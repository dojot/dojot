/* eslint-disable security/detect-non-literal-regexp */
/* eslint-disable security-node/non-literal-reg-expr */
const {
  ConfigManager,
  Kafka: { Consumer },
  Logger,
} = require('@dojot/microservice-sdk');


const config = ConfigManager.getConfig('RETRIEVER');
const logger = new Logger('influxdb-retriever:kafka/DojotConsumer');

/**
   * This class handles messages from dojot topics on kafka
   * @class
   */
class RetrieverConsumer {
  /**
   * Create an instance
   */
  constructor(localPersistence, tenantService, deviceManagerService) {
    logger.debug('constructor: Instantiating a Kafka Consumer for Retrieve');

    this.consumer = new Consumer({
      ...config.sdk,
      'enable.async.commit': true,
      'kafka.consumer': {
        ...config.consumer,
        'group.id': config.consumer['group.id'] + process.env.INSTANCE_ID,
      },
      'kafka.topic': config.topic,
    });

    this.localPersistence = localPersistence;
    this.tenantService = tenantService;
    this.deviceManagerService = deviceManagerService;

    this.idCallbackTenant = null;
    this.idCallbackDeviceManager = null;
  }

  /**
   * Initialize kafka consumer
   * @throws If Cannot init kafka consumer
   */
  async init() {
    try {
      logger.info('init: Kafka starting...');
      await this.consumer.init();
      logger.info('init: ...Kafka started ');
      this.initCallbackForNewTenantEvents();
      this.initCallbackForDeviceEvents();
    } catch (error) {
      logger.error('init: Error starting kafka', error);
      throw new Error('Cannot init kafka consumer');
    }
  }

  /**
   * Instantiates the consumer callback for the tenancy topic
   *
   * @returns callback
   */
  getCallbackForNewTenantEvents() {
    const operations = {
      CREATE: this.tenantService.addNewTenant.bind(this.tenantService),
      DELETE: this.tenantService.deleteTenant.bind(this.tenantService),
    };

    return (data, ack) => {
      try {
        const { value: payload } = data;
        const payloadObject = JSON.parse(payload.toString());

        logger.info('New tenant event received');
        operations[payloadObject.type]({
          id: payloadObject.tenant,
          signatureKey: payloadObject.signatureKey,
        }).then(() => {
          ack();
        }).catch((error) => {
          logger.error(`Dispatch failed. ${error.message}`);
          ack();
        });
      } catch (error) {
        logger.error(error);
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
    logger.debug(`initCallbackForTenantEvents: Register Callbacks for topics with regex ${config.subscribe['topics.regex.tenants']}`);
    const topic = new RegExp(config.subscribe['topics.regex.tenants']);

    this.idCallbackTenant = this.consumer.registerCallback(
      topic, this.getCallbackForNewTenantEvents(),
    );
    logger.debug('registerCallback: Registered Callback');
  }

  /**
   * Instantiates the consumer callback for the device manager topic
   *
   * @returns callback
   */
  getCallbacksForDeviceEvents() {
    return (data, ack) => {
      try {
        const { value: payload } = data;
        const payloadObject = JSON.parse(payload.toString());

        // Mapping the operations
        const operations = {
          create: this.deviceManagerService.addNewDevice.bind(this.deviceManagerService),
          remove: this.deviceManagerService.deleteDevice.bind(this.deviceManagerService),
        };

        if (payloadObject.event === 'create' || payloadObject.event === 'remove') {
          logger.info(`${payloadObject.event} device event received`);
          operations[payloadObject.event](
            // write data to database
            payloadObject,
          ).then(() => {
            ack();
          }).catch((error) => {
            logger.error(`Dispatch failed. ${error.message}`);
            ack();
          });
        }
      } catch (error) {
        logger.error(error);
        ack();
      }
    };
  }

  /*
  * Initializes the consumption of the device manager topic
  *
  * @public
  */
  // eslint-disable-next-line class-methods-use-this
  initCallbackForDeviceEvents() {
    const topic = new RegExp(config.subscribe['topics.regex.device.manager']);

    this.idCallbackDeviceManager = this.consumer.registerCallback(
      topic,
      this.getCallbacksForDeviceEvents(),
    );
  }

  /**
   * A function to get if kafka is connected
   *
   * @returns {Promise<boolean>} if kafka is connect
   */
  async isConnected() {
    try {
      const { connected } = await this.consumer.getStatus();
      if (connected) {
        return true;
      }
      return false;
    } catch (e) {
      logger.error('isConnected:', e);
      return false;
    }
  }

  /**
   * Finishes kafka consumer
   *
   */
  async finish() {
    logger.warn('finish: Finishing Kafka...');
    try {
      await this.consumer.finish();
      this.consumer = null;
      logger.warn('finish: Kafka Consumer finished!');
    } catch (e) {
      logger.error('finish:', e);
    }
  }

  /**
   * Unregister all callbacks
   *
   */
  unregisterCallbacks() {
    if (this.idCallbackTenant) {
      this.consumer.unregisterCallback(this.idCallbackTenant);
      this.idCallbackTenant = null;
      logger.debug('unregisterCallbacks: Unregistered callback for tenant');
    } else {
      logger.warn('unregisterCallbacks: Doesn\'t exist Callback to unregister for tenant');
    }

    if (this.idCallbackDeviceManager) {
      this.consumer.unregisterCallback(this.idCallbackDeviceManager);
      this.idCallbackDeviceManager = null;
      logger.debug('unregisterCallbacks: Unregistered callback for tenant');
    } else {
      logger.warn('unregisterCallbacks: Doesn\'t exist Callback to unregister for devices');
    }
  }

  // eslint-disable-next-line class-methods-use-this
  resume() {
    this.initCallbackForNewTenantEvents();
    this.initCallbackForDeviceEvents();
  }
}

module.exports = RetrieverConsumer;
