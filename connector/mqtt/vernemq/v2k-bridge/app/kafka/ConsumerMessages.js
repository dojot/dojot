const {
  ConfigManager: { getConfig },
  Kafka: { Consumer },
} = require('@dojot/microservice-sdk');
const { killApplication } = require('../Utils');

const {
  sdkconsumer: configSDK,
  consumer: configConsumer,
  subscribe: configSubscribe,
  topic: configTopic,
  healthcheck: configHealthChecker,
} = getConfig('V2K');

/**
   * Class representing an Consumer
   *
   * @class
   */
class ConsumerMessages {
  /**
     * @constructor
     *
     * @param {an instance of @dojot/microservice-sdk.ServiceStateManager
     *          with register service 'v2k-bridge-consumer'} serviceState
     *          Manages the services' states, providing health check and shutdown utilities.
     */
  constructor(tenantService, serviceState, logger) {
    this.serviceState = serviceState;
    this.tenantService = tenantService;
    this.logger = logger;
    this.consumer = null;
    this.idCallbackDeviceManager = null;
    this.idCallbackTenancy = null;
    this.isReady = false;
  }

  async init() {
    try {
      this.consumer = new Consumer({
        ...configSDK,
        'enable.async.commit': true,
        'kafka.consumer': configConsumer,
        'kafka.topic': configTopic,
      });
      // Establishment of communication with the kafka
      await this.consumer.init();
      this.logger.info('Initializing Kafka Consumer...');

      this.createHealthChecker();
      this.registerShutdown();
      this.initCallbackForTenancyEvents();
      this.logger.info('... Kafka Consumer was initialized');
    } catch (error) {
      // something unexpected happended!
      this.logger.error(`Couldn't initialize the Kafka Consumer (${error}).`);
      killApplication();
    }
  }

  /**
   * Instantiates the consumerMessages callback for the tenancy topic
   *
   * @returns callback
   */
  getCallbackForTenancyEvents() {
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
  initCallbackForTenancyEvents() {
    this.logger.debug(`initCallbackForTenantEvents: Register Callbacks for
      topics with regex ${configSubscribe['topics.regex.tenants']}`);
    const topic = new RegExp(configSubscribe['topics.regex.tenants']);

    this.idCallbackTenant = this.consumer
      .registerCallback(topic, this.getCallbackForTenancyEvents());
    this.logger.debug('registerCallback: Registered Callback');
  }

  /**
   * A function to get if kafka is connected
   *
   * @returns {Promise<boolean>} if kafka is connect
   *
   * @public
   */
  async isConnected() {
    try {
      const { connected } = await this.consumer.getStatus();
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
   */
  unregisterCallbacks() {
    if (this.idCallbackDeviceManager) {
      this.consumer.unregisterCallback(this.idCallbackDeviceManager);
      this.idCallbackDeviceManager = null;
      this.logger.debug('unregisterCallbacks: Unregistered callback for tenant');
    } else {
      this.logger.warn('unregisterCallbacks: Doesn\'t exist Callback to unregister for devices');
    }
  }

  createHealthChecker() {
    const healthChecker = async (signalReady, signalNotReady) => {
      if (this.consumer) {
        try {
          const status = await this.consumer.getStatus();
          if (status.connected && !this.isReady) {
            signalReady();
            this.isReady = true;
          } else if (!status.connected && this.isReady) {
            signalNotReady();
            this.isReady = false;
          }
        } catch (error) {
          signalNotReady();
        }
      } else {
        signalNotReady();
      }
    };
    this.serviceState.addHealthChecker(
      'v2k-bridge-consumer',
      healthChecker,
      configHealthChecker['kafka.interval.ms'],
    );
  }

  registerShutdown() {
    this.serviceState.registerShutdownHandler(async () => {
      this.logger.warn('Shutting down Kafka connection...');
      try {
        await this.consumer.finish();
        this.consumer = undefined;
      } catch (error) {
        this.logger.debug(
          'Error while finishing Kafka connection, going on like nothing happened',
        );
      }
    });
  }
}

module.exports = ConsumerMessages;
