const { Logger } = require('@dojot/microservice-sdk');

const { app: appConfig } = require('./config');
const AgentMessenger = require('./AgentMessenger');

// Logger configuration
Logger.setTransport('console', appConfig.logger.transports.console);
Logger.setVerbose(appConfig.logger.verbose);
const logger = new Logger('app');

const agentMessenger = new AgentMessenger();

try {
  agentMessenger.init();
} catch (error) {
  logger.error('An error occurred while initializing the Agent Messenger. Bailing out!');
  logger.error(error);
  process.exit(1);
}
