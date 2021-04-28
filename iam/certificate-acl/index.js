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

logger.info(`Configuration: \n${util.inspect(config, false, 5, true)}`);
if (config.log['file.enable']) {
  const fileLoggerConfig = { level: config.log['file.level'], filename: config.log['file.filename'] };
  Logger.setTransport('file', fileLoggerConfig);
}

const Application = require('./app/App');

// Initializing the service...
(async () => {
  try {
    logger.info('Starting application!');
    const app = new Application();
    await app.init();
    logger.info('Application is running!');
  } catch (err) {
    logger.error('Service will be closed ', err);
    process.kill(process.pid, 'SIGTERM');
  }
})();
