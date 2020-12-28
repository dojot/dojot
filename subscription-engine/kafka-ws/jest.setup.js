const { ConfigManager, Logger } = require('@dojot/microservice-sdk');

const KAFKA_WS_CONFIG_LABEL = 'KAFKA_WS';

ConfigManager.loadSettings(KAFKA_WS_CONFIG_LABEL, 'default.conf');

const config = ConfigManager.getConfig(KAFKA_WS_CONFIG_LABEL);

Logger.setTransport('console', { level: config.log['console.level'] });
Logger.setVerbose(config.log.verbose);
