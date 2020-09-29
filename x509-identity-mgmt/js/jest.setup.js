const { readFileSync } = require('fs');
const { unflatten } = require('flat');
const { Logger } = require('@dojot/microservice-sdk');
const { ConfigManager } = require('@dojot/microservice-sdk');
const Sanitizer = require('@dojot/microservice-sdk/lib/configManager/fileManager/Sanitizer');

/** Mock to fix the problem with the fixed path of the default configuration file. */
require('@dojot/microservice-sdk/lib/configManager/fileManager/Reader').readDefaultConfig = () => {
  const data = readFileSync('./config/default.conf').toString();
  const config = Sanitizer.sanitize(data);
  return config;
};

Logger.setTransport('console');
ConfigManager.loadSettings('X509IDMGMT');
global.config = unflatten(ConfigManager.getConfig('x509idmgmt'));

/* eslint-disable no-console */
global.console = {
  log: jest.fn(), // console.log are ignored in tests

  // Keep native behaviour for other methods, use those to print out
  // things in the tests themselves, not 'console.log'...
  error: console.error,
  warn: console.warn,
  info: console.info,
  debug: console.debug,
};
