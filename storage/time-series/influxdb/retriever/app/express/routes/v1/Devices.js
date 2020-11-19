const { Logger } = require('@dojot/microservice-sdk');
const HttpStatus = require('http-status-codes');
const util = require('util');
const createError = require('http-errors');
const { parseDateTimeToUnixNs } = require('../../../Utils');

const logger = new Logger('influxdb-retriever:express/routes/v1/Device');

/**
 * Checks if "dateFrom" and/or "dateTo" are valid and if they are not returns an error
 *
 * @param {*} req
 * @param {*} res
 * @param {*} next
 */
const isDateValid = (req, res, next) => {
  const {
    dateFrom, dateTo,
  } = req.query;
  if (dateFrom) {
    try {
      parseDateTimeToUnixNs(dateFrom);
    } catch (e) { next(createError(HttpStatus.BAD_REQUEST, `dateFrom: ${e.message}`)); }
  } else if (dateTo) {
    try {
      parseDateTimeToUnixNs(dateTo);
    } catch (e) { next(createError(HttpStatus.BAD_REQUEST, `dateTo: ${e.message}`)); }
  }
  return next();
};

/**
 * Routes to tss endpoints (time series service)
 *
 * @param {string} mountPoint be used as a route prefix
 * @param {Promise<{result: object, totalItems: number}| error>>}
 *                               A promise that returns a result e a totalItems inside that result
 *
 */
module.exports = ({ mountPoint, queryData }) => {
  /**
   * This feature returns data for an attribute with time
   * filter, pagination and order
   */
  const deviceRoute = {
    mountPoint,
    name: 'device-route',
    path: ['/devices/:deviceId/attrs/:attr/data'],
    handlers: [
      {
        method: 'get',
        middleware: [isDateValid, // TODO:  Is it necessary?
          async (req, res) => {
            logger.debug(`device-route.get: req.params=${util.inspect(req.params)}`);
            logger.debug(`device-route.get: req.query=${util.inspect(req.query)}`);

            try {
              const { deviceId, attr } = req.params;
              const {
                dateFrom, dateTo, limit, page, order,
              } = req.query;
              const filters = { dateFrom, dateTo };
              const pagination = { limit, page };
              const {
                result, totalItems,
              } = await queryData(req.tenant, deviceId, attr, filters, pagination, order);
              const paging = req.getPaging(totalItems);
              res.status(HttpStatus.OK).json({ data: result, paging });
            } catch (e) {
              logger.error('device-route.get:', e);
              throw e;
            }
          },
        ],
      },
    ],
  };

  return [deviceRoute];
};
