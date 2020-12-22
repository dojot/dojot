const { Logger } = require('@dojot/microservice-sdk');
const HttpStatus = require('http-status-codes');
const util = require('util');

const logger = new Logger('influxdb-retriever:express/routes/v1/Device');

/**
 * Routes to Devices
 *
 * @param {string} mountPoint be used as a route prefix
 * @param {Promise<{result: object, totalItems: number}| error>>} queryDataByField
 *                               A promise that returns a result e a totalItems inside that result
 * @param {Promise<{result: object, totalItems: number}| error>>} queryDataByMeasurement
 *                               A promise that returns a result e a totalItems inside that result
 */
module.exports = ({ mountPoint, queryDataByField, queryDataByMeasurement }) => {
/**
 * if there is no dateTo, add dateTo to
 * the pagination makes sense even
 * if new values are to be inserted
 */
  const checkDateTo = (req, res, next) => {
    if (!req.query.dateTo) {
      req.query.dateTo = new Date().toISOString();
    }
    return next();
  };

  /**
   * This feature returns data for an device with time
   * filter, pagination and order
   */
  const deviceRoute = {
    mountPoint,
    name: 'device-route',
    path: ['/devices/:deviceId/data'],
    handlers: [
      {
        method: 'get',
        middleware: [
          checkDateTo,
          async (req, res) => {
            logger.debug(`device-route.get: req.params=${util.inspect(req.params)}`);
            logger.debug(`device-route.get: req.query=${util.inspect(req.query)}`);


            try {
              const { deviceId } = req.params;
              const {
                dateFrom, dateTo, limit, page, order,
              } = req.query;

              const filters = { dateFrom, dateTo };
              const pagination = { limit, page };

              const {
                result, totalItems,
              } = await queryDataByMeasurement(req.tenant, deviceId, filters, pagination, order);
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

  /**
   * This feature returns data for an attribute with time
   * filter, pagination and order
   */
  const deviceAttrRoute = {
    mountPoint,
    name: 'device-route-attr',
    path: ['/devices/:deviceId/attrs/:attr/data'],
    handlers: [
      {
        method: 'get',
        middleware: [
          checkDateTo,
          async (req, res) => {
            logger.debug(`device-route-attr.get: req.params=${util.inspect(req.params)}`);
            logger.debug(`device-route-attr.get: req.query=${util.inspect(req.query)}`);

            try {
              const { deviceId, attr } = req.params;
              const {
                dateFrom, dateTo, limit, page, order,
              } = req.query;

              const filters = { dateFrom, dateTo };
              const pagination = { limit, page };

              const {
                result, totalItems,
              } = await queryDataByField(req.tenant, deviceId, attr, filters, pagination, order);
              const paging = req.getPaging(totalItems);
              res.status(HttpStatus.OK).json({ data: result, paging });
            } catch (e) {
              logger.error('device-route-attr.get:', e);
              throw e;
            }
          },
        ],
      },
    ],
  };

  return [deviceRoute, deviceAttrRoute];
};
