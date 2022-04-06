const {
  ServiceStateManager,
  ConfigManager: { transformObjectKeys },
  Logger,
  WebUtils: {
    DojotClientHttp,
  },
} = require('@dojot/microservice-sdk');

const camelCase = require('lodash.camelcase');

const logger = new Logger('basic-auth:App');

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
   * that instantiate basic-auth classes
   */
  constructor(config) {
    logger.debug('constructor: instantiate app...');
    try {
      const dojotClientHttp = new DojotClientHttp({
        defaultClientOptions: {
          timeout: 15000,
        },
        logger,
        defaultRetryDelay: 15000,
        defaultMaxNumberAttempts: 0,
      });

      this.serviceState = new ServiceStateManager({
        lightship: transformObjectKeys(config.lightship, camelCase),
      });
      this.serviceState.registerService('basic-auth-server');
      this.serviceState.registerService('basic-auth-producer');
      this.serviceState.registerService('basic-auth-consumer');
      this.serviceState.registerService('basic-auth-db');

      this.tenantCtrl = new TenantCtrl(Tenant);
      this.basicCredentialsCtrl = new BasicCredentialsCtrl(BasicCredentials, Tenant);
      this.mongoClient = new MongoClient(this.serviceState);
      this.producerMessages = new ProducerMessages(this.serviceState);
      this.consumerMessages = new ConsumerMessages(this.serviceState, this.basicCredentialsCtrl);
      this.server = new HTTPServer(this.serviceState);
      this.deviceService = new DeviceService(config.url, dojotClientHttp);
      this.tenantService = new TenantService(config, dojotClientHttp, logger);
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
    logger.info('init: Initializing the basic-auth...');
    try {
      await this.mongoClient.init();
      await this.producerMessages.init();
      await this.syncLoader.init();
      this.server.registerShutdown();
      this.server.init(
        express(
          [
            AuthenticationRoutes({
              mountPoint: '/basic-auth/v1',
              producerMessages: this.producerMessages,
              basicCredentialsCtrl: this.basicCredentialsCtrl,
            }),
          ],
          this.serviceState,
          this.deviceService,
          this.tenantService,
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
