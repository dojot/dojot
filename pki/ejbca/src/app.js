const bodyParser = require('body-parser');
const express = require('express');
const retryConnection = require('promise-retry');
const { logger } = require('@dojot/dojot-module-logger');

const TAG = { filename: 'app' };
const NodeCache = require('node-cache');

const myCache = new NodeCache({ stdTTL: 0, checkperiod: 0 });
const config = require('./config');

const server = {
  isInitialized: false,
  httpServer: null,
  app: null,
};

/* EJBCA Routes */
const ejbcaRoute = require('../routes/ejbcaRoute');

function initApp(clientEJBCA) {
  server.app = express();
  server.app.use(bodyParser.json({ type: '*/*' }));
  server.app.use(bodyParser.urlencoded({ extended: true }));

  /* Starting the server */
  server.httpServer = server.app.listen(config.ejbcaConf.ejbcaPort, () => {
    logger.debug(`Listening on port ${config.ejbcaConf.ejbcaPort}.`, TAG);
    server.isInitialized = true;
  });

  return retryConnection((retry, number) => {
    logger.debug(`Trying to connect to ejbca wsdl service.. retries: ${number}`, TAG);

    return clientEJBCA.createClient().catch(retry);
  }).then((ejbcaService) => {
    logger.debug('Connected to wsdl service', TAG);

    logger.debug('Setting EJBCA Routes..', TAG);
    ejbcaRoute(server.app, ejbcaService, myCache);
  }).catch((err) => {
    logger.error(err.toString(), TAG);
    return Promise.reject(err);
  });
}

function stopApp() {
  if (server.isInitialized) {
    logger.debug('Stoping the server.');
    server.isInitialized = false;
    server.httpServer.close();
  }
}

module.exports = {
  initApp,
  stopApp,
  server,
};
