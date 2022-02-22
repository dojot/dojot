const {
  Logger,
  ConfigManager,
  WebUtils: { SecretFileHandler },
} = require('@dojot/microservice-sdk');

const App = require('./app/app');

ConfigManager.loadSettings('MICROSERVICEKEYCLOAKSIDECAR', 'default.conf');
const config = ConfigManager.getConfig('MICROSERVICEKEYCLOAKSIDECAR');

const logger = new Logger('microservice-keycloak-sidecar:Server');

Logger.setTransport('console', {
  level: config.logger['console.level'],
});
Logger.setVerbose(config.logger.verbose);

logger.debug('Loading secrets');
const secretHandler = new SecretFileHandler(config, logger);

secretHandler
  .handleCollection(
    [
      'microservicekeycloaksiecar.access.key',
      'microservicekeycloaksiecar.secret.key',
      'keycloak.client.secret',
    ],
    '/secrets/',
  )
  .then(() => {
    // Init Application
    const app = new App(config, logger);
    app
      .init()
      .then(() => {
        logger.info('Server started..');
      })
      .catch((error) => {
        logger.error(error);
      });
    
  });