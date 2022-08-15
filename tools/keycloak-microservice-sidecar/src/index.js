const {
  Logger,
  ConfigManager,
  WebUtils: { SecretFileHandler },
} = require('@dojot/microservice-sdk');

const App = require('./app/web/app');

ConfigManager.loadSettings('KEYCLOAKMICROSERVICESIDECAR', 'default.conf');
const config = ConfigManager.getConfig('KEYCLOAKMICROSERVICESIDECAR');
config.server = {
  ...(config.server),
  ...(config.proxy),
};

const logger = new Logger('microservice-keycloak-sidecar:Server');
Logger.setTransport('console', {
  level: config.logger['console.level'],
});
Logger.setVerbose(config.logger.verbose);
logger.debug('Loading secrets..');

const secretHandler = new SecretFileHandler(config, logger);

secretHandler
  .handleCollection(
    [
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
      .catch((appInitError) => {
        logger.error(appInitError);
      });  
  }).catch((secretHandlerError) => {
    logger.error(secretHandlerError);
    process.kill(process.pid, 'SIGTERM');
  });

process.on('uncaughtException', async (ex) => {
  logger.error(`uncaughtException: Unhandled Exception at: ${ex.stack || ex}. Bailing out!!`);
  process.kill(process.pid, 'SIGTERM');  
});