const { Logger } = require('@dojot/microservice-sdk');

const { app: appCfg } = require('./app/Config');

Logger.setTransport('console', {
  level: appCfg.log.log_console_level,
});
Logger.setVerbose(appCfg.log.log_verbose);
