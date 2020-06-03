/**
 * @overview Initialize a v2k-client creating an Agent Messenger,
 * then initialize it, also set log level for the service.
 * @author Dojot
 */

const { Logger } = require('@dojot/microservice-sdk');

const { app: appConfig } = require('./config');
const AgentMessenger = require('./AgentMessenger');

// Logger configuration
Logger.setTransport('console', appConfig.logger.transports.console);
Logger.setVerbose(appConfig.logger.verbose);

const agentMessenger = new AgentMessenger();

agentMessenger.init();
