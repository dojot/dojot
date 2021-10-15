const {
  WebUtils: {
    framework,
  },
} = require('@dojot/microservice-sdk');

async function validate(path, logger, errorCallback) {
  if (!path) {
    logger.debug('The "path" field is required.');
    await errorCallback();
    throw framework.errorTemplate.BadRequest('The "path" field', 'The "path" field is required.');
  }

  if (path.length < 3 || path.length > 100) {
    logger.debug('The value in the "path" field must be between 3 and 100 characters.');
    await errorCallback();
    throw framework.errorTemplate.BadRequest('The "path" field', 'The value in the "path" field must be between 3 and 100 characters.');
  }

  if (path.startsWith('/.tmp/')) {
    logger.debug('The value in the "path" field is reserved');
    await errorCallback();
    throw framework.errorTemplate.BadRequest('The "path" field', 'The value in the "path" field is reserved');
  }
}

module.exports = {
  validate,
};
