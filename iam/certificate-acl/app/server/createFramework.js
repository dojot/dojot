const {
  WebUtils, Logger,
} = require('@dojot/microservice-sdk');

const logger = new Logger('certificate-acl:express');

module.exports = (aclRoute) => WebUtils.framework.createExpress(
  {
    logger,
    routes: ([
      aclRoute,
    ]),
  },
);
