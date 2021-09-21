/* eslint-disable security/detect-non-literal-regexp */
/* eslint-disable security-node/non-literal-reg-expr */
const {
  ConfigManager,
  Kafka: { Consumer },
  Logger,
  LocalPersistence: {
    InputPersister, InputPersisterArgs,
  },
} = require('@dojot/microservice-sdk');

const config = ConfigManager.getConfig('RETRIEVER');
const logger = new Logger('influxdb-retriever:kafka/DojotConsumer');

const INPUT_PERSISTER_CONFIG = {
  levels: [
    {
      type: 'static',
      name: 'tenants',
      options: {
        keyEncoding: 'utf8',
        valueEncoding: 'Bool',
      },
    },
    {
      type: 'dynamic',
      source: 'meta.service',
      options: {
        keyEncoding: 'utf8',
        valueEncoding: 'Bool',
      },
    },
  ],
  frames: [
    {
      level: 0,
      pair: {
        key: {
          type: 'dynamic',
          source: 'tenant',
        },
        value: {
          type: 'static',
          source: true,
        },
      },
    },
    {
      level: 1,
      pair: {
        key: {
          type: 'dynamic',
          source: 'data.id',
        },
        value: {
          type: 'static',
          source: true,
        },
      },
    },
  ],
};

/**
   * This class handles messages from dojot topics on kafka
   * @class
   */
class RetrieverConsumer {
  /**
     * Create an instance
     */
  constructor(localPersistence) {
    logger.debug('constructor: Instantiating a Kafka Consumer for Retrieve');

    this.consumer = new Consumer({
      ...config.sdk,
      'kafka.consumer': config.consumer,
      'kafka.topic': config.topic,
    });
    this.localPersistence = localPersistence;
    this.inputPersister = new InputPersister(localPersistence, INPUT_PERSISTER_CONFIG);

    this.idCallbackTenant = null;
    this.idCallbackDeviceManager = null;
    this.idCallbackDeviceData = null;
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
    } catch (error) {
      logger.error('init: Error starting kafka', error);
      throw new Error('Cannot init kafka consumer');
    }
  }

  initCallbackForNewTenantEvents() {
    const topicSuffix = config.subscribe['topics.suffix.tenants'];
    logger.debug(`initCallbackForTenantEvents: Register Callbacks for topics with suffix ${topicSuffix}`);
    // TODO: better understand why this regex is unsafe and change it
    const topic = new RegExp(`^.+${topicSuffix.replace(/\./g, '\\.')}`);

    const callback = async (data) => {
      const { value: payload } = data;
      const payloadObject = JSON.parse(payload.toString());
      if (payloadObject.type === 'CREATE') {
        logger.info('New tenant event received');
        this.inputPersister.dispatch(
          payloadObject, InputPersisterArgs.INSERT_OPERATION,
        ).then(() => {
          this.registerCallbacksForDeviceEvents(payloadObject.tenant);
        }).catch((error) => {
          logger.error(`Dispatch failed. ${error.message}`);
        });
      }
    };

    this.idCallbackTenant = this.consumer.registerCallback(topic, callback);
    logger.debug('registerCallback: Registered Callback');
  }

  // eslint-disable-next-line class-methods-use-this
  registerCallbacksForDeviceEvents(tenant) {
    const topicSuffix = config.subscribe['topics.suffix.device.manager'];

    const callback = async (data) => {
      const { value: payload } = data;
      const payloadObject = JSON.parse(payload.toString());

      const opTypes = {
        create: InputPersisterArgs.INSERT_OPERATION,
        delete: InputPersisterArgs.DELETE_OPERATION,
      };

      if (payloadObject.event === 'create' || payloadObject.event === 'delete') {
        logger.info(`${payloadObject.event} tenant event received`);
        this.inputPersister.dispatch(
          JSON.parse(payload.toString()), opTypes[payloadObject.event],
        ).catch((error) => {
          logger.error(`Dispatch failed. ${error.message}`);
        });
      }
    };

    this.idCallbackTenant = this.consumer.registerCallback(`${tenant}.${topicSuffix}`, callback);
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
     *  @throws If Cannot finish
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
     * @throws If Cannot unregister callback
     */
  unregisterCallbacks() {
    try {
      if (this.idCallbackTenant) {
        this.consumer.unregisterCallback(this.idCallbackTenant);
        this.idCallbackTenant = null;
        logger.debug('unregisterCallbacks: Unregistered callback for tenant');
      } else {
        logger.warn('unregisterCallbacks: Doesn\'t exist Callback to unregister for tenant');
      }
    } catch (e) {
      logger.error('unregisterCallbacks:', e);
    }
  }
}

module.exports = RetrieverConsumer;
