const { getHTTPRouter, logger } = require('@dojot/dojot-module-logger');
const bodyParser = require('body-parser');
const express = require('express');

const config = require('../Config');
const routes = require('./Routes');

const TAG = { filename: 'express/App' };

/**
 * Express Server
 */
class ExpressApp {
  /**
   * Init config for Express Server
   */
  constructor() {
    logger.debug('Starting ExpressApp - Constructor ...', TAG);

    this.isInitialized = false;
    this.httpServer = null;
    this.app = express();
    this.app.use(bodyParser.json({ type: '*/*' }));
    this.app.use(bodyParser.urlencoded({ extended: true }));
    this.app.use(getHTTPRouter());
    this.app.use('/', routes);
  }

  /**
   * Init listening
   */
  initListen() {
    logger.debug('Starting Listening on port...', TAG);
    this.httpServer = this.app.listen(config.prom.port, () => {
      logger.info(`Listening on port ${config.prom.port}.`, TAG);
      this.isInitialized = true;
    });
  }

  /**
   * Stop listening
   */
  stop() {
    if (this.isInitialized) {
      logger.info('Stopping the server...', TAG);

      this.isInitialized = false;
      this.httpServer.close();
    }
  }
}

module.exports = ExpressApp;
