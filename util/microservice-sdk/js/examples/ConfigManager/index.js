/* eslint-disable no-console */
const util = require('util');
const { ConfigManager } = require('@dojot/microservice-sdk');
const camelCase = require('lodash/camelCase');

// Creating some environment variables for tests
process.env.K2V_PRODUCER_KAFKA_CBA = 'kafka_test';
process.env.K2V_PRODUCER_KAFKA_AEHO = 'kafka_teste2';
process.env.K2V_APP_HOSTNAME = 'k2v-bridge';

// These environment variables replace the values in the user/default config file
// process.env.K2V_KAFKA_BROKERS = '["broker3", "broker4:9092"]';
// process.env.K2V_APP_CONNECTION_RETRY_COUNT = 3;

// Creates the configuration for K2V
ConfigManager.createConfig('K2V');
const config = ConfigManager.getConfig('K2V');
console.log('Built config:');
console.log(util.inspect(config, { depth: 10, colors: true }));
console.log('========================================');

// External lib function key transformation example
const newConfig = ConfigManager.transformObjectKeys(config.app, camelCase);
console.log('App config converted to camelCase:');
console.log(util.inspect(newConfig, { depth: 10, colors: true }));
console.log('========================================');

// User created function key transformation example
const producerConfig = ConfigManager.transformObjectKeys(
  config.producer,
  (value) => value.replace(/\./, '_'),
);
console.log('Producer config converted from dotted to "underscored":');
console.log(util.inspect(producerConfig, { depth: 10, colors: true }));
