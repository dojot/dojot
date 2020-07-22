const { map, pickBy } = require('lodash');
const { toCanonicalFileFormat } = require('../Utils');

/**
 * Creates the configuration string array from environment variables.
 *
 * @param {string} service acronym for the service, should only contain letters/numbers
 *
 * @returns {string[]}
 */
const parseEnvironmentVariables = (service) => {
  // Picking only the variables that begin with '<service>_'
  const environmentVariables = pickBy(
    process.env, (value, key) => key.toString().match(`${service}_.*`),
  );

  const config = map(environmentVariables, (value, key) => {
    // Removing the '<service>_' from the variable name and replacing '_' by '.'
    const newKey = key.toString().replace(`${service}_`, '').toLowerCase().replace(/_/g, '.');
    return toCanonicalFileFormat(value, newKey);
  });

  return config;
};

module.exports = { parseEnvironmentVariables };
