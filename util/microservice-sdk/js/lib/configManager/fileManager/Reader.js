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
 * @throws
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
 * @param {string} path
 * @param {string} filename
 *
 * @returns {string[]}
 */
const readUserConfig = (path, filename) => {
  const fileLocation = Utils.createFilename(filename, path);
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
 * @param {string} path
 *
 * @returns {{}}
 */
const readJson = (service, path) => {
  const filename = Utils.createFilename(`${service}.json`, path);
  if (existsSync(filename)) {
    const data = readFileSync(filename);
    const config = JSON.parse(data);
    return config;
  }
  logger.debug('JSON configuration file not present');
  return {};
};

module.exports = { readDefaultConfig, readJson, readUserConfig };
