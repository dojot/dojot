const {
  WebUtils, Logger
} = require('@dojot/microservice-sdk');

const { mockingHistoryRoute } = require('./express/mockingRoute');

const { notificationRoute } = require('./express/notificationRoute');


const logger = new Logger('history-proxy:express');

const framework = WebUtils.framework.createExpress(
  {
    logger: logger,
    routes: ([
      mockingHistoryRoute,
      notificationRoute
    ]),
    errorHandlers: [WebUtils.framework.defaultErrorHandler(
      { logger: logger, }
    )
    ],
    catchInvalidRequest: true,
  });


module.exports = { framework };