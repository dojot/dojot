const {
  ServiceStateManager,
  Logger,
  ConfigManager: { transformObjectKeys, getConfig },
  WebUtils: {
    DojotClientHttp,
  },
} = require('@dojot/microservice-sdk');

const camelCase = require('lodash.camelcase');

const {
  lightship: configLightship,
  url: configURL,
} = getConfig('HTTP_AGENT');

const serviceState = new ServiceStateManager({
  lightship: transformObjectKeys(configLightship, camelCase),
});
serviceState.registerService('http-server');
serviceState.registerService('http-producer');
serviceState.registerService('http-redis');
serviceState.registerService('http-agent-consumer');
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

const dojotHttpclient = new DojotClientHttp({
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
    this.config = config;
    try {
      this.server = new Server(serviceState);
      this.producerMessages = new ProducerMessages(serviceState);
      this.redisManager = new RedisManager(serviceState);
      this.consumerMessages = new ConsumerMessages(serviceState, this.redisManager);
      this.tenantService = new TenantService({
        keycloakConfig: config.keycloak,
        dojotHttpclient,
        logger,
      });
      this.certificateAclService = new CertificateAclService(configURL['certificate.acl'], dojotHttpclient);
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
    this.deviceAuthService = new DeviceAuthService(
      this.tenantService,
      this.config.url['device.auth'],
      dojotHttpclient,
    );
    logger.info('init: Initializing the http-agent...');
    try {
      await this.producerMessages.init();
      await this.consumerMessages.init();
      this.redisManager.init();
      await this.consumerMessages.init();
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
