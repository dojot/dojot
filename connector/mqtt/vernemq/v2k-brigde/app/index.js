/**
 * @overview Initialize a v2k-client creating an Agent Messenger,
 * then initialize it, also set log level for the service.
 * @author Dojot
 */

const { logger } = require('@dojot/dojot-module-logger');

const config = require('./config');
const AgentMessenger = require('./AgentMessenger');

/* set log level */
logger.setLevel(config.app.logLevel);

const agentMessenger = new AgentMessenger(config);
agentMessenger.init();
