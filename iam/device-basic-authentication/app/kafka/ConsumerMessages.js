const {
  ConfigManager: { getConfig },
  Kafka: { Consumer },
  Logger,
} = require('@dojot/microservice-sdk');
const { killApplication } = require('../Utils');

const logger = new Logger('basic-auth:Consumer');

const {
  sdkconsumer: configSDK,
  consumer: configConsumer,
  subscribe: configSubscribe,
  topic: configTopic,
  healthchecker: configHealthChecker,
} = getConfig('BASIC_AUTH');

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
   *          with register service 'basic-auth-consumer'} serviceState
   *          Manages the services' states, providing health check and shutdown utilities.
   */
  constructor(serviceState, basicCredentials, tenantService) {
    this.serviceState = serviceState;
    this.basicCredentials = basicCredentials;
    this.tenantService = tenantService;
    this.consumer = null;
    this.idCallbackTenant = null;
    this.idCallbackDeviceManager = null;
    this.isReady = false;
  }

  async init() {
    try {
      this.consumer = new Consumer({
        ...configSDK,
        'kafka.consumer': configConsumer,
        'kafka.topic': configTopic,
      });
      // Establishment of communication with the kafka
      await this.consumer.init();
      logger.info('Initializing Kafka Consumer...');

      this.createHealthChecker();
      this.registerShutdown();
      this.initCallbackForNewTenantEvents();
      this.initCallbackForDeviceEvents();
      logger.info('... Kafka Consumer was initialized');
    } catch (error) {
      // something unexpected happended!
      logger.error(`Couldn't initialize the Kafka Consumer (${error}).`);
      killApplication();
    }
  }

  /**
   * Instantiates the consumerMessages callback for the tenancy topic
   *
   * @returns callback
   */
  getCallbackForTenantEvents() {
    return async (data) => {
      try {
        const { value: payload } = data;
        const payloadObject = JSON.parse(payload.toString());

        if (payloadObject.type === 'CREATE') {
          await this.tenantService.create({
            signatureKey: payloadObject.signatureKey,
            id: payloadObject.tenant,
          });
        } else if (payloadObject.type === 'DELETE') {
          await this.tenantService.remove(payloadObject.tenant);
          logger.info(`${payloadObject.type} tenant event received`);
          logger.debug(payloadObject);
          await this.basicCredentials.removeAllFromTenant(payloadObject.tenant);
        }
      } catch (error) {
        logger.error(error);
      }
    };
  }

  /*
      * Initializes the consumption of the tenancy topic
      *
      * @public
      */
  initCallbackForNewTenantEvents() {
    logger.debug(`initCallbackForTenantEvents: Register Callbacks for topics with regex ${configSubscribe['topics.regex.tenants']}`);
    const topic = RegExp(configSubscribe['topics.regex.tenants']);

    this.idCallbackTenant = this.consumer.registerCallback(
      topic,
      this.getCallbackForTenantEvents(),
    );
    logger.debug('registerCallback: Registered Callback');
  }

  /**
       * Instantiates the consumerMessages callback for the device manager topic
       *
       * @returns callback
       */
  getCallbacksForDeviceEvents() {
    return async (data) => {
      try {
        const { value: payload } = data;
        const payloadObject = JSON.parse(payload.toString());
        logger.info(`payloadObject: ${payload.toString()}`);
        if (payloadObject.event === 'remove') {
          logger.info(`payloadObject: ${payloadObject}`);
          logger.info(`${payloadObject.event} device event received`);
          logger.debug(payloadObject);
          await this.basicCredentials.remove(payloadObject.meta.service, payloadObject.data.id);
        }
      } catch (error) {
        logger.error(error);
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
    const topic = RegExp(configSubscribe['topics.regex.device.manager']);

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
      'basic-auth-consumer',
      healthChecker,
      configHealthChecker['kafka.interval.ms'],
    );
  }

  registerShutdown() {
    this.serviceState.registerShutdownHandler(async () => {
      logger.warn('Shutting down Kafka connection...');
      try {
        await this.consumer.finish();
        this.consumer = undefined;
      } catch (error) {
        logger.debug(
          'Error while finishing Kafka connection, going on like nothing happened',
        );
      }
    });
  }
}

module.exports = ConsumerMessages;
