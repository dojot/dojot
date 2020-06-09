const { Logger } = require('@dojot/microservice-sdk');

const util = require('util');

const Config = require('./config');
const AgentMessenger = require('./AgentMessenger');

// Logger configuration
Logger.setTransport('console', Config.app.logger.transports.console);
Logger.setVerbose(Config.app.logger.verbose);
const logger = new Logger('app');

logger.info(`Configuration:\n${util.inspect(Config, false, 5, true)}`);

const agentMessenger = new AgentMessenger();

try {
  agentMessenger.init();
} catch (error) {
  logger.error('An error occurred while initializing the Agent Messenger. Bailing out!');
  logger.error(error);
  process.exit(1);
}
