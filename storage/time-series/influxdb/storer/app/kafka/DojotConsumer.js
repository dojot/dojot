const {
  ConfigManager,
  Kafka: { Consumer },
  Logger,
} = require('@dojot/microservice-sdk');

const config = ConfigManager.getConfig('STORER');

const logger = new Logger('influxdb-storer:kafka/DojotConsumer');

/**
 * This class handle messages from dojot topics on kafka
 * @class
 */
class DojotConsumer {
  /**
   * Create a instance
   */
  constructor() {
    logger.debug('constructor: Instance Kafka Consumer for Dojot');

    this.consumer = new Consumer({
      ...config.sdk,
      'kafka.consumer': config.consumer,
      'kafka.topic': config.topic,
    });

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

  /**
   * Register callbacks to handle with tenants  events for create and delete
   *
   * @param {async function(string)} callbackCreate Receive the tenant name created
   * @param {async function(string)} callbackDelete Receive the tenant name deleted
   */
  registerCallbackTenants(callbackCreate, callbackDelete = null) {
    const topicSuffix = config.subscribe['topics.suffix.tenants'];
    logger.debug(`registerCallbackTenants: Register Callbacks for topics with suffix ${topicSuffix}`);
    const topic = new RegExp(`^.+${topicSuffix.replace(/\./g, '\\.')}`);
    const callback = async (data) => {
      try {
        const { value: payload } = data;
        logger.debug(`registerCallbackTenants: Receiving data ${payload.toString()}`);
        const payloadObj = JSON.parse(payload);
        const { type, tenant } = payloadObj;
        switch (type) {
          case 'CREATE':
            if (!tenant) {
              logger.warn('registerCallbackTenants: CREATE missing tenant');
            } else {
              await callbackCreate(tenant);
            }
            break;
          case 'DELETE':
            if (callbackDelete) {
              if (!tenant) {
                logger.warn('registerCallbackTenants: CREATE missing tenant');
              } else {
                await callbackDelete(tenant);
              }
            } else {
              logger.debug('registerCallbackTenants: callbackDelete not enable');
            }
            break;
          default:
            logger.debug('registerCallbackTenants: event was discarded ');
        }
      } catch (e) {
        logger.error('registerCallbackTenants:', e);
      }
    };
    this.idCallbackTenant = this.consumer.registerCallback(topic, callback);
    logger.debug('registerCallback: Registered Callback');
  }

  /**
   * Register callbacks to handle with device manager events for configure
   * (when send data from dojot to a device) and when remove a device from dojot
   *
   * @param {async function(string, string, number|string, object)} callbackConfig
   *                              Receive tenant, deviceid, timestamp (unix timestamp ms
   *                              or a string restricted ISO 8601 (YYYY-MM-DDThh:mm:ss.fffffffffZ),
   *                              attrs (key:value)
   * @param {async function(string, string)} callbackDeleteDevice Receive tenant,
   *                                         deviceid (default = null)
   */
  registerCallbackDeviceManager(callbackConfig, callbackDeleteDevice = null) {
    const topicSuffix = config.subscribe['topics.suffix.device.manager'];
    logger.debug(`registerCallbackDeviceManager: Register Callback for topics with suffix ${topicSuffix}`);
    const topic = new RegExp(`^.+${topicSuffix.replace(/\./g, '\\.')}`);

    const callback = async (data) => {
      try {
        const { value: payload } = data;
        logger.debug(`registerCallbackDeviceManager: Receiving data ${payload.toString()}`);
        const {
          event,
          meta: { service: tenant, timestamp },
          data: { id: deviceid, attrs },
        } = JSON.parse(payload);
        switch (event) {
          case 'configure':
            if (!deviceid) {
              logger.warn('registerCallbackDeviceManager: configure missing deviceid');
            } else if (!tenant) {
              logger.warn('registerCallbackDeviceManager: configure missing tenant');
            } else if (!attrs) {
              logger.warn('registerCallbackDeviceManager: configure missing attrs');
            } else if (DojotConsumer.checkIfShouldPersist(attrs)) {
              await callbackConfig(tenant, deviceid, timestamp, attrs);
            } else {
              logger.debug('registerCallbackDeviceManager: shouldPersist is false');
            }
            break;
          case 'remove':
            if (!deviceid) {
              logger.warn('registerCallbackDeviceManager: remove missing deviceid');
            } else if (!tenant) {
              logger.warn('registerCallbackDeviceManager: remove missing tenant');
            } else if (callbackDeleteDevice) {
              await callbackDeleteDevice(tenant, deviceid);
            } else {
              logger.debug('registerCallbackDeviceManager: callbackDelete not enable');
            }
            break;
          default:
            logger.debug(`registerCallbackDeviceManager: ${event} event was discarded `);
        }
      } catch (e) {
        logger.error('registerCallbackDeviceManager:', e);
      }
    };
    this.idCallbackDeviceManager = this.consumer.registerCallback(topic, callback);
    logger.debug('registerCallbackDeviceManager: Registered Callback');
  }

  /**
   * Register callbacks to handle with device data events
   *
   * @param {async function(string, string, date??, object)} callbackDeviceData
   *                              Receive tenant, deviceid, timestamp, attrs
   *
   */
  registerCallbackDeviceData(callbackDeviceData) {
    const topicSuffix = config.subscribe['topics.suffix.device.data'];
    logger.debug(`registerCallbackDeviceData: Register Callback for topic with suffix ${topicSuffix}`);
    const topic = new RegExp(`^.+${topicSuffix.replace(/\./g, '\\.')}`);

    const callback = async (data) => {
      try {
        const { value: payload } = data;
        logger.debug(`registerCallbackDeviceData: Receiving data ${payload.toString()}`);
        const {
          metadata:
          { deviceid, tenant, timestamp },
          attrs,
        } = JSON.parse(payload);
        if (!deviceid) {
          logger.warn('registerCallbackDeviceData: missing deviceid');
        } else if (!tenant) {
          logger.warn('registerCallbackDeviceData: missing tenant');
        } else if (!attrs) {
          logger.warn('registerCallbackDeviceData: missing attrs');
        } else if (DojotConsumer.checkIfShouldPersist(attrs)) {
          await callbackDeviceData(tenant, deviceid, timestamp, attrs);
        } else {
          logger.debug('registerCallbackDeviceData: shouldPersist is false');
        }
      } catch (e) {
        logger.error('registerCallbackDeviceData:', e);
      }
    };
    this.idCallbackDeviceData = this.consumer.registerCallback(topic, callback);
    logger.debug('registerCallbackDeviceData: Registered Callback');
  }

  /**
   * Check if `attrs` has a key shouldPersist and if exist if its value is true
   *
   * @param {object} attrs  Object key:value
   * @returns {boolean} If should persist
   */
  static checkIfShouldPersist(attrs) {
    return !Object.prototype.hasOwnProperty.call(attrs, 'shouldPersist')
      || (attrs.shouldPersist);
  }

  /**
 * A function to get if kafka is connected
 * @param {*} timeout Timeout to get the status (optional)
 */
  isConnected(timeout = 3000) {
    return new Promise((resolve) => {
      this.consumer.consumer.getMetadata({ timeout }, (err) => {
        if (err) {
          resolve(false);
        } else {
          resolve(true);
        }
      });
    });
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
        logger.debug('unregisterCallbacks: Unregistered callback for tenant');
      } else {
        logger.warn('unregisterCallbacks: Doesn\'t exist Callback to unregister for tenant');
      }
      if (this.idCallbackDeviceManager) {
        this.consumer.unregisterCallback(this.idCallbackDeviceManager);
        logger.debug('unregisterCallbacks: Unregistered Callback for DeviceManager');
      } else {
        logger.warn('unregisterCallbacks: Doesn\'t exist Callback to unregister for DeviceManager');
      }

      if (this.idCallbackDeviceData) {
        this.consumer.unregisterCallback(this.idCallbackDeviceData);
        logger.debug('unregisterCallbacks: Unregistered Callback for DeviceData');
      } else {
        logger.warn('unregisterCallbacks: Doesn\'t exist Callback to unregister for DeviceData');
      }
    } catch (e) {
      logger.error('unregisterCallbacks:', e);
      throw new Error('Cannot unregister callback');
    }
  }
}

module.exports = DojotConsumer;
