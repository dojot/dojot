const {
  ConfigManager: { getConfig, loadSettings, transformObjectKeys },
  Logger,
  ServiceStateManager,
  WebUtils: { SecretFileHandler },
} = require('@dojot/microservice-sdk');

const camelCase = require('lodash.camelcase');

const util = require('util');

const { killApplication } = require('./app/Utils');

/*
 * TODO: the idea is to create a default environment variable that will be used internally by the
 * ConfigManager to automatically select the user config file. Since this is not something we care
 * too much now, this will do the trick.
 */
const userConfigFile = process.env.V2K_APP_USER_CONFIG_FILE || 'production.conf';
// Creating the configuration
loadSettings('V2K', userConfigFile);
const config = getConfig('V2K');

// Logger configuration
Logger.setVerbose(config.log.verbose);
Logger.setTransport('console', { level: config.log['console.level'] });
if (config.log['file.enable']) {
  Logger.setTransport('file', {
    level: config.log['file.level'],
    filename: config.log['file.filename'],
  });
}
const logger = new Logger('v2k-bridge');

logger.info(`Configuration:\n${util.inspect(config, false, 5, true)}`);

const serviceState = new ServiceStateManager({
  lightship: transformObjectKeys(config.lightship, camelCase),
});

const App = require('./app/App');

const secretFileHandler = new SecretFileHandler(config, logger);

// Initializing the service...
secretFileHandler.handle('keycloak.client.secret', '/secrets/').then(async () => {
  try {
    logger.info('Initializing...');
    const app = new App(config, logger, serviceState);
    await app.init();
  } catch (err) {
    logger.error('Service will be closed', err);
    killApplication();
  }
});
