/**
 * @file index.js
 * @dependencies [ util, @dojot/microservice-sdk ]
 */

const util = require('util');
const {
  ConfigManager: { getConfig, loadSettings },
  Logger,
} = require('@dojot/microservice-sdk');

const CERTIFICATE_ACL_CONFIG_LABEL = 'CERTIFICATE_ACL';

const userConfigFile = process.env.CERTIFICATE_ACL_USER_CONFIG_FILE || 'production.conf';

loadSettings(CERTIFICATE_ACL_CONFIG_LABEL, userConfigFile);

const config = getConfig(CERTIFICATE_ACL_CONFIG_LABEL);

/**
 * Log configuration
 */
Logger.setVerbose(config.log.verbose);
Logger.setTransport('console', { level: config.log['console.level'] });
const logger = new Logger('app');

logger.info(`Configurarion: \n${util.inspect(config, false, 5, true)}`);

const application = require('./app/App');

application.init();
