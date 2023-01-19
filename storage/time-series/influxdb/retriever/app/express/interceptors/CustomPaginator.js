const util = require('util');
const { Logger } = require('@dojot/microservice-sdk');
const paginate = require('express-paginate');
const createError = require('http-errors');
const HttpStatus = require('http-status-codes');

const logger = new Logger('influxdb-retriever:express/interceptors/CustomPaginator');

/**
 * Express middleware to paginate data
 *
 * @param {object} config  An object with the key limit, the default  a
 *                         Number to limit results returned per page ,
 *                         and maxLimit a Number to restrict the number
 *                         of results returned to per page
 */
module.exports = ({ defaultLimit, maxLimit }) => ({
  name: 'custom-paginator-interceptor',
  // eslint-disable-next-line no-useless-escape
  path: ['/tss/v1/devices', '/tss/v1/query', '/tss/v1/fluxquery'],
  middleware: [(req, res, next) => {
    // check if was passed a value greater than the configured maximum limit
    logger.debug(`CustomPaginator1: req.params=${util.inspect(req.params)}`);
    logger.debug(`CustomPaginator1: req.query=${util.inspect(req.query)}`);
    if (req.query.limit > maxLimit) {
      next(createError(HttpStatus.BAD_REQUEST, `The configured maximum limit is ${maxLimit}`));
    }
    next();
  },
  paginate.middleware(defaultLimit, maxLimit),
  (req, res, next) => {
    logger.debug(`CustomPaginator2: req.params=${util.inspect(req.params)}`);
    logger.debug(`CustomPaginator2: req.query=${util.inspect(req.query)}`);

    // disable limit=0 to avoid getting infinite (all) results
    if (req.query.limit < 1) req.query.limit = defaultLimit;

    /**
       * Create the object to be returned along with the data when a
       * paginated result is requested.
       * @param {Number} currentTotalItemsPage Total items displayed on the current page
       */
    req.getPaging = (currentTotalItemsPage) => {
      const previousPage = (!res.locals.paginate.hasPreviousPages) ? null
        : {
          number: req.query.page - 1,
          url: res.locals.paginate.href(true),
        };

      const currentPage = {
        number: req.query.page,
        url: res.locals.paginate.href()
          .replace(`page=${req.query.page + 1}`, `page=${req.query.page}`),
      };

      // To avoid counts
      // there may be a next empty page
      const nextPage = (currentTotalItemsPage < req.query.limit) ? null
        : {
          number: req.query.page + 1,
          url: res.locals.paginate.href(false),
        };

      return {
        previous: previousPage,
        current: currentPage,
        next: nextPage,
      };
    };
    next();
  }],
});
