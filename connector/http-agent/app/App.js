const {
  ServiceStateManager,
  ConfigManager: { getConfig, transformObjectKeys },
  Logger,
} = require('@dojot/microservice-sdk');

const camelCase = require('lodash.camelcase');

const { lightship: configLightship } = getConfig('HTTP-AGENT');

const serviceState = new ServiceStateManager({
  lightship: transformObjectKeys(configLightship, camelCase),
});
serviceState.registerService('http-server');
serviceState.registerService('http-producer');
serviceState.registerService('http-cache');

const logger = new Logger('http-agent:App');

const Server = require('./Server');
const ProducerMessages = require('./ProducerMessages');
const Cache = require('./Cache');

const express = require('./express');
const incomingMessagesRoutes = require('./express/routes/v1/IncomingMessages');

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
      this.cache = new Cache(serviceState);
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
      this.cache.init();
      await this.producerMessages.init();
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
          this.cache,
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
