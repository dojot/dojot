/**
 * @module Writer writes the JSON config file.
 */
const { writeFileSync } = require('fs');

const jsonStringify = require('fast-safe-stringify');

const { createFilename } = require('../Utils');
const { Logger } = require('../../logging/Logger');

const logger = new Logger('microservice-sdk:config-manager-writer');

/**
 * Writes the JSON configuration object in a file.
 *
 * @param {string} service
 * @param {string} path
 * @param {{}} data object to be written in a file
 */
const writeJson = (service, path, data) => {
  try {
    const jsonFilename = createFilename(`${service}.json`, path);
    writeFileSync(jsonFilename, jsonStringify(data));
  } catch (error) {
    logger.error(`${error.stack || error}`);
  }
};

module.exports = { writeJson };
