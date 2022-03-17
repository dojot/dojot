const {
  ServiceStateManager,
  Logger,
  ConfigManager: { transformObjectKeys },
  WebUtils: {
    DojotClientHttp,
  },
} = require('@dojot/microservice-sdk');

const camelCase = require('lodash.camelcase');

const logger = new Logger('http-agent:App');

const Server = require('./Server');
const ProducerMessages = require('./kafka/ProducerMessages');
const ConsumerMessages = require('./kafka/ConsumerMessages');
const RedisManager = require('./redis/RedisManager');
const DeviceAuthService = require('./axios/DeviceAuthService');
const CertificateAclService = require('./axios/CertificateAclService');
const TenantService = require('./axios/TenantService');

const express = require('./express');
const incomingMessagesRoutes = require('./express/routes/v1/IncomingMessages');

const dojotClientHttp = new DojotClientHttp({
  defaultClientOptions: {
    timeout: 15000,
  },
  logger,
  defaultRetryDelay: 15000,
  defaultMaxNumberAttempts: 0,
});

/**
 * Wrapper to initialize the service
 */
class App {
  /**
   * Constructor App
   * that instantiate http-agent classes
   */
  constructor(config) {
    logger.debug('constructor: instantiate app...');
    try {
      this.serviceState = new ServiceStateManager({
        lightship: transformObjectKeys(config.lightship, camelCase),
      });
      this.serviceState.registerService('http-server');
      this.serviceState.registerService('http-producer');
      this.serviceState.registerService('http-redis');
      this.server = new Server(this.serviceState);
      this.producerMessages = new ProducerMessages(this.serviceState);
      this.redisManager = new RedisManager(this.serviceState);
      this.tenantService = new TenantService({
        keycloakConfig: config.keycloak,
        dojotClientHttp,
        logger,
      });
      this.consumerMessages = new ConsumerMessages(this.tenantService, config, logger);
      this.deviceAuthService = new DeviceAuthService(config.url['device.auth'], dojotClientHttp);
      this.certificateAclService =
        new CertificateAclService(config.url['certificate.acl'], dojotClientHttp);
    } catch (e) {
      logger.error('constructor:', e);
      throw e;
    }
  }

  /**
   * Initialize the server and producer
   */
  async init() {
    await this.tenantService.loadTenants();
    logger.info('init: Initializing the http-agent...');
    try {
      await this.producerMessages.init();
      await this.consumerMessages.init();
      this.redisManager.init();
      this.server.registerShutdown();

      this.server.init(
        express(
          [
            incomingMessagesRoutes({
              mountPoint: '/http-agent/v1',
              producerMessages: this.producerMessages,
            }),
          ],
          this.serviceState,
          this.redisManager,
          this.deviceAuthService,
          this.certificateAclService,
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
