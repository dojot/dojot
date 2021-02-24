const responseHandler = require('./handlers/handleResponse');
const influxHandler = require('./handlers/handleRequest');
const validationHandler = require('./handlers/handleValidation');

const createDataToBePassed = (req) => {
  const request = {
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
    request.limit = req.query.lastN;
    request.order = 'desc';
  }
  if (req.query.firstN) {
    request.limit = req.query.firstN;
    request.order = 'asc';
  }
  return request;
};

const handleHistoryRequest = (req) => {
  const request = createDataToBePassed(req);

  // using "chain of responsability" pattern.
  return validationHandler
    .handle(request)
    .then(influxHandler.handle)
    .then(responseHandler.handle);
  // errors will be caught by the main thread.
};

module.exports = { handleHistoryRequest, createDataToBePassed };
