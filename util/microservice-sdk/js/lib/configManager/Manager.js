const Parsers = require('./parsers');
const { Reader, Writer } = require('./fileManager');
const Merger = require('./Merger');

/**
 * Creates the configuration file ./config/<service>.conf. The precedence is (from higher to lower):
 * - Environment variables
 * - User config file
 * - Default config file
 *
 * @param {string} service acronym for the service, should only contain letters/numbers
 * @param {string} path path to the user configuration file, defaults to './config'
 */
const loadSettings = (service, path = './config', userConfigFile = 'production.conf') => {
  const envVarsData = Parsers.EnvVars.parseEnvironmentVariables(service);
  const userData = Reader.readUserConfig(path, userConfigFile);
  const defaultData = Reader.readDefaultConfig();

  const config = Merger.mergeConfigs(envVarsData, userData, defaultData);

  Writer.writeJson(service, path, config);
};

/**
 * Retrieves the configuration from the file.
 *
 * @param {string} service acronym for the service, should only contain letters/numbers
 * @param {string} path path to the user configuration file, defaults to './config'
 *
 * @returns {{}} the configuration object
 */
const getConfig = (service, path = './config') => {
  const config = Reader.readJson(service, path);
  return config;
};

module.exports = { loadSettings, getConfig };
