/**
 * @file index.js
 * @dependencies [ util, @dojot/microservice-sdk ]
 */

const util = require('util');
const {
  ConfigManager: { getConfig, loadSettings },
  Logger,
  WebUtils: { SecretFileHandler },
} = require('@dojot/microservice-sdk');
const DIContainer = require('./app/DIContainer');

const CERTIFICATE_ACL_CONFIG_LABEL = 'CERTIFICATE_ACL';

const userConfigFile = process.env.CERTIFICATE_ACL_USER_CONFIG_FILE || 'production.conf';

loadSettings(CERTIFICATE_ACL_CONFIG_LABEL, userConfigFile);

const config = getConfig(CERTIFICATE_ACL_CONFIG_LABEL);
const container = DIContainer();

/**
 * Log configuration
 */
Logger.setVerbose(config.logger.verbose);
Logger.setTransport('console', { level: config.logger['console.level'] });
const logger = container.resolve('logger');

logger.info(`Configuration: \n${util.inspect(config, false, 5, true)}`);
if (config.logger['file.enable']) {
  const fileLoggerConfig = { level: config.logger['file.level'], filename: config.logger['file.filename'] };
  Logger.setTransport('file', fileLoggerConfig);
}

const Application = require('./app/App');

const secretFileHandler = new SecretFileHandler(config, logger);

secretFileHandler.handle('keycloak.client.secret', '/secrets/').then(async () => {
  try {
    logger.info('Starting application!');
    const app = new Application();
    await app.init();
    logger.info('Application is running!');
  } catch (err) {
    logger.error('Service will be closed: ', err);
    process.kill(process.pid, 'SIGTERM');
  }
});
