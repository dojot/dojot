const { merge } = require('lodash');
const Parsers = require('./parsers');

/**
 * Merges the configurations from environment variables, user config file and default config file
 * into one object.
 *
 * @param {string[]} envVarsData
 * @param {string[]} userData
 * @param {string[]} defaultData
 */
const mergeConfigs = (envVarsData, userData, defaultData) => {
  // Transforming the lines in parsed objects
  const parsedEnvVars = envVarsData.map(Parsers.Type.parseLine);
  const parsedUser = userData.map(Parsers.Type.parseLine);
  const parsedDefault = defaultData.map(Parsers.Type.parseLine);

  // Applying the types to parameters, taking into account the types in the default configuration
  // file
  const typedEnvVars = parsedEnvVars
    .map((parsedLine) => Parsers.Type.mapToTyped(parsedLine, parsedDefault));
  const typedUser = parsedUser
    .map((parsedLine) => Parsers.Type.mapToTyped(parsedLine, parsedDefault));
  const typedDefault = parsedDefault
    .map((parsedLine) => Parsers.Type.applyType(parsedLine));

  // Building the object that can be written to a .json file
  const objectEnvVars = Parsers.File.parseConfig(typedEnvVars);
  const objectUser = Parsers.File.parseConfig(typedUser);
  const objectDefault = Parsers.File.parseConfig(typedDefault);

  return merge(objectDefault, objectUser, objectEnvVars);
};

module.exports = { mergeConfigs };
