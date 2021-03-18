const {
  WebUtils, Logger,
} = require('@dojot/microservice-sdk');

const { historyRoute } = require('./express/historyRoute');

const { notificationRoute } = require('./express/notificationRoute');

const logger = new Logger('history-proxy:express');

/**
* Creates Express wrapper   
*/
const framework = WebUtils.framework.createExpress(
  {
    logger,
    routes: ([
      historyRoute,
      notificationRoute
    ]),
  }
);


module.exports = { framework };