const handleResponse = require('./handlers/handleResponse');

const handleRequest = require('./handlers/handleRequest');

const handleValidation = require('./handlers/handleValidation');

/**
* Create the Data to be passed in pipeline following the
* chain of responsibility pattern
*/
const createDataToBePassed = (req) => {
  const pipelineData = {
    deviceId: req.params.deviceId,
    attr: req.query.attr,
    dateTo: req.query.dateTo,
    dateFrom: req.query.dateFrom,
    limit: 0,
    isAllAttrs: false,
    isMultipleAttr: false,
    order: 'desc',
    headers: req.headers.authorization,
    rawResponse: '',
  };

  if (req.query.lastN) {
    pipelineData.limit = req.query.lastN;
    pipelineData.order = 'desc';
  }
  if (req.query.firstN) {
    pipelineData.limit = req.query.firstN;
    pipelineData.order = 'asc';
  }
  return pipelineData;
};

/**
 * Handle the History requests
 *
 * @param {object} req
 * @return {object} Promise
 */
const handleHistoryRequest = (req) => {
  const pipelineData = createDataToBePassed(req);

  // using "chain of responsability" pattern.
  return handleValidation
    .handle(pipelineData)
    .then(handleRequest.handle)
    .then(handleResponse.handle);
  // errors will be caught by the main thread.
};

module.exports = { handleHistoryRequest, createDataToBePassed };
