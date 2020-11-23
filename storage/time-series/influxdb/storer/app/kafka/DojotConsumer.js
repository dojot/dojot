const {
  ConfigManager,
  Kafka: { Consumer },
  Logger,
} = require('@dojot/microservice-sdk');

const config = ConfigManager.getConfig('STORER');

const logger = new Logger('influxdb-storer:kafka/DojotConsumer');

/**
 * This class handles messages from dojot topics on kafka
 * @class
 */
class DojotConsumer {
  /**
   * Create an instance
   */
  constructor() {
    logger.debug('constructor: Instantiating a Kafka Consumer for Dojot');

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
   * Registers callbacks to handle tenant events (creation and removal)
   *
   * @param {async function(string)} handleTenantCreateEvent Receive the tenant name created
   * @param {async function(string)} handleTenantDeleteEvent Receive the tenant name deleted
   */
  registerCallbackForTenantEvents(handleTenantCreateEvent, handleTenantDeleteEvent = null) {
    const topicSuffix = config.subscribe['topics.suffix.tenants'];
    logger.debug(`registerCallbackForTenantEvents: Register Callbacks for topics with suffix ${topicSuffix}`);
    const topic = new RegExp(`^.+${topicSuffix.replace(/\./g, '\\.')}`);
    const callback = async (data) => {
      try {
        const { value: payload } = data;
        logger.debug(`registerCallbackForTenantEvents: Receiving data ${payload.toString()}`);
        const payloadObj = JSON.parse(payload);
        const { type, tenant } = payloadObj;
        switch (type) {
          case 'CREATE':
            if (!tenant) {
              logger.warn('registerCallbackForTenantEvents: CREATE - missing tenant');
            } else {
              await handleTenantCreateEvent(tenant);
            }
            break;
          case 'DELETE':
            if (handleTenantDeleteEvent) {
              if (!tenant) {
                logger.warn('registerCallbackForTenantEvents: DELETE - missing tenant');
              } else {
                await handleTenantDeleteEvent(tenant);
              }
            } else {
              logger.debug('registerCallbackForTenantEvents: callbackDelete not enable');
            }
            break;
          default:
            logger.debug('registerCallbackForTenantEvents: event was discarded ');
        }
      } catch (e) {
        logger.error('registerCallbackForTenantEvents:', e);
      }
    };
    this.idCallbackTenant = this.consumer.registerCallback(topic, callback);
    logger.debug('registerCallback: Registered Callback');
  }

  /**
   * Registers callbacks to handle device management events (removal and configuration).
   *
   * @param {async function(string, string, number|string, object)} handleDeviceConfigurationEvent
   *                              Receive tenant, deviceid, date-time (unix timestamp ms
   *                              or RFC3339,
   *                              attrs (key:value)
   * @param {async function(string, string)} handleDeviceRemoveEvent Receive tenant,
   *                                         deviceid (default = null)
   */
  registerCallbacksForDeviceMgmtEvents(
    handleDeviceConfigurationEvent, handleDeviceRemoveEvent = null,
  ) {
    const topicSuffix = config.subscribe['topics.suffix.device.manager'];
    logger.debug(`registerCallbacksForDeviceMgmtEvents: Register Callback for topics with suffix ${topicSuffix}`);
    const topic = new RegExp(`^.+${topicSuffix.replace(/\./g, '\\.')}`);

    const callback = async (data) => {
      try {
        const { value: payload } = data;
        logger.debug(`registerCallbacksForDeviceMgmtEvents: Receiving data - ${payload.toString()}`);
        const {
          event,
          meta: { service: tenant, timestamp },
          data: { id: deviceid, attrs },
        } = JSON.parse(payload);
        switch (event) {
          case 'configure':
            await DojotConsumer.handleData(
              deviceid,
              tenant,
              timestamp,
              attrs,
              handleDeviceConfigurationEvent,
            );
            break;
          case 'remove':
            if (!deviceid) {
              logger.warn('registerCallbacksForDeviceMgmtEvents: remove - missing deviceid');
            } else if (!tenant) {
              logger.warn('registerCallbacksForDeviceMgmtEvents: remove - missing tenant');
            } else if (handleDeviceRemoveEvent) {
              await handleDeviceRemoveEvent(tenant, deviceid);
            } else {
              logger.debug('registerCallbacksForDeviceMgmtEvents: callbackDelete not enable');
            }
            break;
          default:
            logger.debug(`registerCallbacksForDeviceMgmtEvents: ${event} event was discarded `);
        }
      } catch (e) {
        logger.error('registerCallbacksForDeviceMgmtEvents:', e);
      }
    };
    this.idCallbackDeviceManager = this.consumer.registerCallback(topic, callback);
    logger.debug('registerCallbacksForDeviceMgmtEvents: Registered Callback');
  }

  /**
   * Registers callbacks to handle device data events.
   *
   * @param {async function(string, string, number|string, object)} handleDeviceData
   *                              Receive tenant, deviceid,  date-time (unix timestamp ms
   *                              or RFC3339, attrs
   *
   */
  registerCallbacksForDeviceDataEvents(handleDeviceData) {
    const topicSuffix = config.subscribe['topics.suffix.device.data'];
    logger.debug(`registerCallbacksForDeviceDataEvents: Register Callback for topic with suffix ${topicSuffix}`);
    const topic = new RegExp(`^.+${topicSuffix.replace(/\./g, '\\.')}`);

    const callback = async (data) => {
      try {
        const { value: payload } = data;
        logger.debug(`registerCallbacksForDeviceDataEvents: Receiving data - ${payload.toString()}`);
        const {
          metadata:
          { deviceid, tenant, timestamp },
          attrs,
        } = JSON.parse(payload);
        await DojotConsumer.handleData(deviceid, tenant, timestamp, attrs, handleDeviceData);
      } catch (e) {
        logger.error('registerCallbacksForDeviceDataEvents:', e);
      }
    };
    this.idCallbackDeviceData = this.consumer.registerCallback(topic, callback);
    logger.debug('registerCallbacksForDeviceDataEvents: Registered Callback');
  }

  /**
   * Check if `attrs` has a key shouldPersist and if exist if its value is true
   *
   * @param {object} attrs  Object key:value
   * @returns {boolean} false if should persists exists and its value is false and true otherwise.
   */
  static checkIfShouldPersist(attrs) {
    return !Object.prototype.hasOwnProperty.call(attrs, 'shouldPersist')
      || (attrs.shouldPersist);
  }

  /**
   *
   * Checks if deviceId, tenant, attrs are not null,
   * if these attributes should persist and remove shouldPersist from attrs,
   * if all checks are ok call the promise handleData.
   *
   * @param {string} deviceid the deviceid that issued the event
   * @param {string} tenant the tenant that issued the event
   * @param {string|number} timestamp date-time (unix timestamp ms or RFC3339,
   * @param {Object} attrs Object of type key value (key:value)
   * @param {async function(string, string, number|string, object)} handleData
   *                              Receive tenant, deviceid, date-time (unix timestamp ms
   *                              or RFC3339,
   *                              attrs (key:value)
   */
  static async handleData(deviceid, tenant, timestamp, attrs, handleData) {
    const attrsCopy = { ...attrs };
    if (!deviceid) {
      logger.warn('registerCallbacksForDeviceMgmtEvents: configure - missing deviceid');
    } else if (!tenant) {
      logger.warn('registerCallbacksForDeviceMgmtEvents: configure - missing tenant');
    } else if (!attrs) {
      logger.warn('registerCallbacksForDeviceMgmtEvents: configure - missing attrs');
    } else if (DojotConsumer.checkIfShouldPersist(attrsCopy)) {
      delete attrsCopy.shouldPersist;
      await handleData(tenant, deviceid, timestamp, attrsCopy);
    } else {
      logger.debug('registerCallbacksForDeviceMgmtEvents: shouldPersist is false');
    }
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
   * Finish kafka consumer
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
    }
  }
}

module.exports = DojotConsumer;
