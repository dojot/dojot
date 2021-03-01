/**
 * @file index.js
 * @dependencies [ util, @dojot/microservice-sdk ]
 */

const util = require('util');
const { ConfigManager, Logger } = require('@dojot/microservice-sdk');

const DOJOT_ACL_CONFIG_LABEL = 'DOJOT_ACL';

const userConfigFile = process.env.DOJOT_ACL_APP_USSER_CONFIG_FILE || 'production.conf';

ConfigManager.loadSettings(DOJOT_ACL_CONFIG_LABEL, userConfigFile);

const config = ConfigManager.getConfig(DOJOT_ACL_CONFIG_LABEL);

Logger.setTransport('console', { level: config.log['console.level'] });

/**
 * Log configuration
 */

Logger.setVerbose(config.log.verbose);
const logger = new Logger('app');

logger.info(`Configurarion: \n${util.inspect(config, false, 5, true)}`);

const application = require('./app/App');

application.init();
