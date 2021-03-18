const { readFileSync } = require('fs');
const { ConfigManager } = require('@dojot/microservice-sdk');
const Sanitizer = require('@dojot/microservice-sdk/lib/configManager/fileManager/Sanitizer');
const filename = require('require-main-filename')();

/** Mock to fix the problem with the fixed path of the default configuration file. */

require('@dojot/microservice-sdk/lib/configManager/fileManager/Reader').readDefaultConfig = () => {
  const data = readFileSync('./config/default.conf').toString();
  return Sanitizer.sanitize(data);
};

ConfigManager.loadSettings('HISTORYPROXY', 'production.conf', './config', filename);
const configs = ConfigManager.getConfig('HISTORYPROXY', './config', filename);

ConfigManager.getConfig = jest.fn(() => configs);
