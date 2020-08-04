/**
 * @module Reader reads the config files.
 */
const { readFileSync, existsSync } = require('fs');
const path = require('path');

const Sanitizer = require('./Sanitizer');
const Utils = require('../Utils');
const { Logger } = require('../../logging/Logger');

const logger = new Logger('microservice-sdk:config-manager-reader');

// Obtains the absolute path of the initial script
const rootPath = path.dirname(require.main.filename);
// Default configuration file location
const DefaultConfigLocation = path.join(rootPath, 'config/default.conf');

/**
 * Reads the default configuration file and returns an array of unparsed parameters.
 *
 * @returns {string[]}
 * @throws if the default configuration file is empty or not found
 */
const readDefaultConfig = () => {
  if (existsSync(DefaultConfigLocation)) {
    const data = readFileSync(DefaultConfigLocation).toString();

    if (data === '') {
      throw new Error('empty default configuration');
    }

    const config = Sanitizer.sanitize(data);
    return config;
  }

  throw new Error(`default configuration file ${DefaultConfigLocation} not found`);
};

/**
 * Reads the user configuration file and returns an array of unparsed parameters.
 *
 * @param {string} filePath
 * @param {string} filename
 *
 * @returns {string[]}
 */
const readUserConfig = (filePath, filename) => {
  const fileLocation = Utils.createFilename(filename, filePath);
  if (existsSync(fileLocation)) {
    const data = readFileSync(fileLocation).toString();
    const config = Sanitizer.sanitize(data);
    return config;
  }
  logger.debug('User configuration not present');
  return [];
};

/**
 * Reads the configuration from the JSON file.
 *
 * @param {string} service
 * @param {string} filePath
 *
 * @returns {{}}
 */
const readJson = (service, filePath) => {
  const filename = Utils.createFilename(`${service}.json`, filePath);
  if (existsSync(filename)) {
    const data = readFileSync(filename);
    const config = JSON.parse(data);
    return config;
  }
  logger.debug('JSON configuration file not present');
  return {};
};

module.exports = { readDefaultConfig, readJson, readUserConfig };
