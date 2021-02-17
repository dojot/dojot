const { Logger } = require('@dojot/microservice-sdk');

const logger = new Logger('gui-proxy');

const handle = (r) => {
  const newR = { ...r };
  if (r.attr === undefined) {
    newR.isAllAttrs = true;
  }
  if (Array.isArray(r.attr)) {
    [newR.attr] = r.attr;
    newR.isMultipleAttr = true;
  }
  if (r.dateTo === undefined || r.dateTo === null) {
    newR.dateTo = new Date().toISOString();
  }
  if (r.dateFrom === undefined || r.dateFrom === null) {
    newR.dateFrom = '1970-01-01T00:00:00.000Z';
  }
  if (r.limit === undefined || r.limit === null || r.limit === 0) {
    newR.limit = 256; // 2999
  }

  logger.info('Params were sent correctly.');
  return Promise.resolve(newR);
};

module.exports = { handle };
