const {
  ConfigManager: { getConfig, loadSettings, transformObjectKeys },
  Logger,
  ServiceStateManager,
} = require('@dojot/microservice-sdk');

const util = require('util');
const camelCase = require('lodash.camelcase');

const AgentMessenger = require('./app/AgentMessenger');
const MQTTClient = require('./app/MQTTClient');
const Utils = require('./app/Utils');

/*
 * TODO: the idea is to create a default environment variable that will be used internally by the
 * ConfigManager to automatically select the user config file. Since this is not something we care
 * to much now, this will do the trick.
 */
const userConfigFile = process.env.K2V_APP_USER_CONFIG_FILE || 'production.conf';
loadSettings('K2V', userConfigFile);
const config = getConfig('K2V');

// Logger configuration
Logger.setVerbose(config.log.verbose);
Logger.setTransport('console', { level: config.log['console.level'] });
if (config.log['file.enable']) {
  Logger.setTransport('file', {
    level: config.log['file.level'],
    filename: config.log['file.filename'],
  });
}
const logger = new Logger('k2v:index');

logger.info(`Configuration:\n${util.inspect(config, false, 5, true)}`);

const serviceStateManager = new ServiceStateManager({
  lightship: transformObjectKeys(config.lightship, camelCase),
});
const agentMessenger = new AgentMessenger(serviceStateManager);
const mqttClient = new MQTTClient(agentMessenger, serviceStateManager);

// Registering the services, shutdown handlers and health checkers
serviceStateManager.registerService('kafka');
serviceStateManager.registerService('mqtt');
serviceStateManager.registerShutdownHandler(agentMessenger.shutdownHandler.bind(agentMessenger));
serviceStateManager.registerShutdownHandler(mqttClient.shutdownHandler.bind(mqttClient));
serviceStateManager.addHealthChecker(
  'kafka',
  agentMessenger.healthChecker.bind(agentMessenger),
  config.healthcheck['kafka.interval.ms'],
);

try {
  mqttClient.init();
} catch (error) {
  logger.error('An error occurred while initializing the MQTT Client. Bailing out!');
  logger.error(error.stack || error);
  Utils.killApplication();
}
