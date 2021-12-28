const {
  ServiceStateManager,
  ConfigManager: { getConfig, transformObjectKeys },
  Logger,
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

const logger = new Logger('http-agent:App');

const Server = require('./Server');
const ProducerMessages = require('./ProducerMessages');
const RedisManager = require('./redis/RedisManager');
const DeviceAuthService = require('./axios/DeviceAuthService');
const CertificateAclService = require('./axios/CertificateAclService');

const express = require('./express');
const incomingMessagesRoutes = require('./express/routes/v1/IncomingMessages');
const axios = require('./axios/createAxios');

/**
 * Wrapper to initialize the service
 */
class App {
  /**
   * Constructor App
   * that instantiate http-agent classes
   */
  constructor() {
    logger.debug('constructor: instantiate app...');
    try {
      this.server = new Server(serviceState);
      this.producerMessages = new ProducerMessages(serviceState);
      this.redisManager = new RedisManager(serviceState);
      this.deviceAuthService = new DeviceAuthService(configURL['device.auth'], axios);
      this.certificateAclService = new CertificateAclService(configURL['certificate.acl'], axios);
    } catch (e) {
      logger.error('constructor:', e);
      throw e;
    }
  }

  /**
   * Initialize the server and producer
   */
  async init() {
    logger.info('init: Initializing the http-agent...');
    try {
      await this.producerMessages.init();
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
          serviceState,
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
