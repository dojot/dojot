const { Logger } = require('@dojot/microservice-sdk');

const util = require('util');
const { unflatten } = require('flat');

const config = require('./config');
const AgentMessenger = require('./AgentMessenger');

// Logger configuration
const logConfig = unflatten(config.logger);
Logger.setTransport('console', logConfig.transports.console);
Logger.setVerbose(logConfig.verbose);
const logger = new Logger('app');

logger.info(`Configuration:\n${util.inspect(config, false, 5, true)}`);

const agentMessenger = new AgentMessenger();

try {
  agentMessenger.init();
} catch (error) {
  logger.error('An error occurred while initializing the Agent Messenger. Bailing out!');
  logger.error(error);
  process.exit(1);
}
