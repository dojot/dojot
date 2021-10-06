const {
  WebUtils: { framework },
} = require('@dojot/microservice-sdk');

/**
 * Returns the type informed in the accept header
 *
 * @param {string} accept the accept entered in the request
 * @returns {"json"|"csv"}  the format the data will be returned
 */
const getAcceptableType = (req) => {
  const allowedTypes = ['json', 'csv'];
  const accept = req.accepts(allowedTypes);
  if (!accept) {
    throw framework.errorTemplate.NotAcceptable('Not Acceptable', 'Not Acceptable');
  }
  return accept;
};

module.exports = { getAcceptableType };
