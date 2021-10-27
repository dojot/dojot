const {
  WebUtils: {
    framework,
  },
} = require('@dojot/microservice-sdk');

/**
 * Validate a path according to rules
 *
 * @param {*} path the file path
 * @param {*} logger the logger
 * @param {*} errorCallback a function that will be called before an error is thrown.
 */
async function validate(path, logger, errorCallback) {
  if (!path) {
    logger.debug('The "path" field is required.');
    if (errorCallback) {
      await errorCallback();
    }
    throw framework.errorTemplate.BadRequest('The "path" field is required.', 'The "path" field is required.');
  }

  if (path.length < 3 || path.length > 100) {
    logger.debug('The value in the "path" field must be between 3 and 100 characters.');
    if (errorCallback) {
      await errorCallback();
    }
    throw framework.errorTemplate.BadRequest('The "path" field is invalid.', 'The value in the "path" field must be between 3 and 100 characters.');
  }

  if (path.startsWith('/.tmp/')) {
    logger.debug('The value in the "path" field is reserved');
    if (errorCallback) {
      await errorCallback();
    }
    throw framework.errorTemplate.BadRequest('The "path" field is invalid.', 'The value in the "path" field is reserved.');
  }
}

module.exports = {
  validate,
};
