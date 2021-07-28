const { Logger } = require('@dojot/microservice-sdk');
const HttpStatus = require('http-status-codes');
const util = require('util');
const DeviceDataServ = require('../../services/v1/DeviceDataService');
const { getExpectedResponseFormat } = require('../../helpers/AcceptHeaderHelper');

const logger = new Logger('influxdb-retriever:express/routes/v1/Device');


/**
 * Routes to Devices
 *
 * @param {string} mountPoint be used as a route prefix
 * @param {Promise<{result: object, totalItems: number}| error>>} queryDataByField
 *                               A promise that returns a result and a totalItems inside that result
 * @param {Promise<{result: object, totalItems: number}| error>>} queryDataByMeasurement
 *                               A promise that returns a result and a totalItems inside that result
 */
module.exports = ({ mountPoint, queryDataByField, queryDataByMeasurement }) => {
  const deviceDataServ = new DeviceDataServ(queryDataByField, queryDataByMeasurement);

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
              const { accept } = req.headers;
              const {
                dateFrom, dateTo, limit, page, order,
              } = req.query;

              const [result, paging] = await deviceDataServ.getDeviceData(
                req.tenant, deviceId, dateFrom, dateTo, limit, page, order, req.getPaging,
              );

              if (accept && getExpectedResponseFormat(accept) === 'csv') {
                return res.status(HttpStatus.OK).send(DeviceDataServ.parseDeviceDataToCsv(result));
              }

              return res.status(HttpStatus.OK).json({ data: result, paging });
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
              const { accept } = req.headers;

              const {
                dateFrom, dateTo, limit, page, order,
              } = req.query;

              const [result, paging] = await deviceDataServ.getDeviceAttrData(
                req.tenant, deviceId, attr, dateFrom, dateTo, limit, page, order, req.getPaging,
              );

              if (accept && getExpectedResponseFormat(accept) === 'csv') {
                return res.status(HttpStatus.OK).send(
                  DeviceDataServ.parseDeviceAttrDataToCsv(result),
                );
              }

              return res.status(HttpStatus.OK).json({ data: result, paging });
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
