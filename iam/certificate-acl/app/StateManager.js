const { ServiceStateManager } = require('@dojot/microservice-sdk');
const { getConfig, transformObjectKeys } = require('@dojot/microservice-sdk/lib/configManager');
const camelCase = require('lodash.camelcase');

const { lightship } = getConfig('CERTIFICATE_ACL');
const config = { lightship: transformObjectKeys(lightship, camelCase) };
const serviceStateManager = new ServiceStateManager(config);
serviceStateManager.registerService('kafka');
serviceStateManager.registerService('redis');

module.exports = serviceStateManager;
