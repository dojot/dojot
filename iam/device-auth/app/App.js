const {
  ServiceStateManager,
  ConfigManager: { getConfig, transformObjectKeys },
  Logger,
} = require('@dojot/microservice-sdk');

const camelCase = require('lodash.camelcase');

const {
  lightship: configLightship,
  url: configURL,
} = getConfig('DEVICE_AUTH');

const serviceState = new ServiceStateManager({
  lightship: transformObjectKeys(configLightship, camelCase),
});
serviceState.registerService('device-auth-server');
serviceState.registerService('device-auth-producer');
serviceState.registerService('device-auth-consumer');
serviceState.registerService('device-auth-db');

const logger = new Logger('device-auth:App');

const HTTPServer = require('./server/HTTPServer');
const ProducerMessages = require('./kafka/ProducerMessages');
const ConsumerMessages = require('./kafka/ConsumerMessages');
const MongoClient = require('./db/MongoClient');
const BasicCredentialsCtrl = require('./db/controllers/BasicCredentialsCtrl');
const TenantCtrl = require('./db/controllers/TenantCtrl');
const SyncLoader = require('./sync/SyncLoader');

const express = require('./server/express');
const AuthenticationRoutes = require('./server/express/routes/Authentication');
const DeviceService = require('./axios/DeviceService');
const TenantService = require('./axios/TenantService');

const BasicCredentials = require('./db/models/BasicCredentials');
const Tenant = require('./db/models/Tenant');

/**
 * Wrapper to initialize the service
 */
class App {
  /**
   * Constructor App
   * that instantiate device-auth classes
   */
  constructor() {
    logger.debug('constructor: instantiate app...');
    try {
      this.tenantCtrl = new TenantCtrl(Tenant);
      this.basicCredentialsCtrl = new BasicCredentialsCtrl(BasicCredentials, Tenant);
      this.mongoClient = new MongoClient(serviceState);
      this.producerMessages = new ProducerMessages(serviceState);
      this.consumerMessages = new ConsumerMessages(serviceState, this.basicCredentialsCtrl);
      this.server = new HTTPServer(serviceState);
      this.deviceService = new DeviceService(configURL);
      this.tenantService = new TenantService(configURL.tenants);
      this.syncLoader = new SyncLoader(
        this.deviceService,
        this.tenantService,
        this.consumerMessages,
        this.basicCredentialsCtrl,
        this.tenantCtrl,
      );
    } catch (e) {
      logger.error('constructor:', e);
      throw e;
    }
  }

  /**
   * Initialize the server and producer
   */
  async init() {
    logger.info('init: Initializing the device-auth...');
    try {
      await this.mongoClient.init();
      await this.producerMessages.init();
      await this.syncLoader.init();
      this.server.registerShutdown();
      this.server.init(
        express(
          [
            AuthenticationRoutes({
              mountPoint: '/device-auth/v1',
              producerMessages: this.producerMessages,
              basicCredentialsCtrl: this.basicCredentialsCtrl,
            }),
          ],
          serviceState,
          this.deviceService,
        ),
      );
    } catch (e) {
      logger.error('init:', e);
      throw e;
    }
    logger.info('init:...service initialized.');
  }
}

module.exports = App;
