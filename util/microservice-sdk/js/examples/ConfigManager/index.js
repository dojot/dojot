const util = require('util');
const { ConfigManager } = require('@dojot/microservice-sdk');

// Creating some environment variables for tests
process.env.K2V_PRODUCER_KAFKA_CBA = 'kafka_test';
process.env.K2V_PRODUCER_KAFKA_AEHO = 'kafka_teste2';
process.env.K2V_APP_HOSTNAME = 'k2v-bridge';

// These environment variables replace the values in the user/default config file
// process.env.K2V_KAFKA_BROKERS = '["broker3", "broker4:9092"]';
// process.env.K2V_APP_CONNECTION_RETRY_COUNT = 3;

// Creates the configuration for K2V
ConfigManager.createConfig('K2V');
// eslint-disable-next-line no-console
console.log(util.inspect(ConfigManager.getConfig('K2V'), { depth: 10, colors: true }));
