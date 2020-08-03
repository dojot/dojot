/**
 * @module Writer writes the JSON config file.
 */
const { writeFileSync } = require('fs');

const jsonStringify = require('fast-safe-stringify');

const { createFilename } = require('../Utils');

/**
 * Writes the JSON configuration object in a file.
 *
 * @param {string} service
 * @param {string} path
 * @param {{}} data object to be written in a file
 */
const writeJson = (service, path, data) => {
  const jsonFilename = createFilename(`${service}.json`, path);
  writeFileSync(jsonFilename, jsonStringify(data));
};

module.exports = { writeJson };
