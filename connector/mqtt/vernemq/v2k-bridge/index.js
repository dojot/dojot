const {
  ConfigManager: { getConfig, loadSettings, transformObjectKeys },
  Logger,
  ServiceStateManager,
} = require('@dojot/microservice-sdk');

const camelCase = require('lodash.camelcase');
const util = require('util');

const AgentMessenger = require('./app/AgentMessenger');
const MQTTClient = require('./app/MQTTClient');

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
const logger = new Logger('app');

logger.info(`Configuration:\n${util.inspect(config, false, 5, true)}`);

const manager = new ServiceStateManager({
  lightship: transformObjectKeys(config.lightship, camelCase),
});
const agentMessenger = new AgentMessenger();
const mqttClient = new MQTTClient(agentMessenger, manager);

manager.registerService('kafka');
manager.addHealthChecker(
  'kafka',
  agentMessenger.healthChecker.bind(agentMessenger),
  config.healthcheck['kafka.interval.ms'],
);

manager.registerShutdownHandler(agentMessenger.shutdownHandler.bind(agentMessenger));
manager.registerShutdownHandler(mqttClient.shutdownHandler.bind(mqttClient));

try {
  agentMessenger.init(mqttClient);
} catch (error) {
  logger.error('An error occurred while initializing the Agent Messenger. Bailing out!');
  logger.error(error.stack || error);
  process.exit(1);
}
