const { logger } = require('@dojot/dojot-module-logger');

const config = require('./config');
const AgentMessenger = require('./AgentMessenger');

/* set log level */
logger.setLevel(config.app.mqttLogLevel);

const agentMessenger = new AgentMessenger(config);
agentMessenger.init();
