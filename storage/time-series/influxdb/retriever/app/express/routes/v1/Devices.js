const {
  ConfigManager: { getConfig },
  Logger,
  WebUtils: { framework },
} = require('@dojot/microservice-sdk');
const { graphqlHTTP } = require('express-graphql');
const HttpStatus = require('http-status-codes');

const util = require('util');
const DeviceDataServ = require('../../services/v1/DeviceDataService');
const AcceptHeaderHelper = require('../../helpers/AcceptHeaderHelper');

const logger = new Logger('influxdb-retriever:express/routes/v1/Devices');

const { graphql: { graphiql } } = getConfig('RETRIEVER');
const rootSchema = require('../../../graphql/Schema');


/**
 * Routes to Devices
 *
 * @param {string} mountPoint be used as a route prefix
 *
 * @param {Promise<{result: object, totalItems: number}| error>>} QueryDataUsingGraphql
 *                               A promise that returns result in graphQL format
 *
 * @param {Promise<{result: object, totalItems: number}| error>>} queryDataByField
 *                               A promise that returns a result and a totalItems inside that result
 * @param {Promise<{result: object, totalItems: number}| error>>} queryDataByMeasurement
 *                               A promise that returns a result and a totalItems inside that result
 */
module.exports = ({
  localPersistence, mountPoint, queryDataUsingGraphql, queryDataByField, queryDataByMeasurement,
}) => {
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
              const accept = AcceptHeaderHelper.getAcceptableType(req);

              try {
                await localPersistence.get(req.tenant, deviceId);
              } catch (error) {
                throw framework.errorTemplate.NotFound(`Not found ${req.tenant}/${deviceId}`);
              }

              const {
                dateFrom, dateTo, limit, page, order,
              } = req.query;

              const [result, paging] = await deviceDataServ.getDeviceData(
                req.tenant, deviceId, dateFrom, dateTo, limit, page, order, req.getPaging,
              );

              if (accept === 'csv') {
                return res.status(HttpStatus.OK).send(DeviceDataServ.parseDeviceDataToCsv(result));
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
              const accept = AcceptHeaderHelper.getAcceptableType(req);

              try {
                await localPersistence.get(req.tenant, deviceId);
              } catch (error) {
                throw framework.errorTemplate.NotFound(`Not found ${req.tenant}/${deviceId}`);
              }

              const {
                dateFrom, dateTo, limit, page, order,
              } = req.query;

              const [result, paging] = await deviceDataServ.getDeviceAttrData(
                req.tenant, deviceId, attr, dateFrom, dateTo, limit, page, order, req.getPaging,
              );

              if (accept === 'csv') {
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


  /**
   * This endpoint returns data fetched using graphql schema
   */
  const deviceGraphqlRoute = {
    mountPoint,
    name: 'graphql-route',
    path: ['/devices/graphql'],
    handlers: [
      {
        method: 'get',
        middleware: [
          graphqlHTTP({
            schema: rootSchema,
            graphiql,
            rootValue: {
              async getData(
                root,
                params,
              ) {
                // param 'context' won't be used
                logger.debug(`graphql-route.get: graphql query=${util.inspect(root)}`);
                const {
                  filter:
                  {
                    range: { start, stop = '' },
                    limit = 10,
                    devices,
                    isDesc = true,
                  },
                } = root;

                // creating filters
                const filters = {
                  dateFrom: start,
                  dateTo: stop,
                };
                const order = isDesc ? 'desc' : 'asc';
                const page = { limit };

                // request data
                try {
                  const res = await queryDataUsingGraphql(
                    params.tenant,
                    devices,
                    filters,
                    page,
                    order,
                  );
                  return res;
                } catch (e) {
                  logger.error('graphql-route.get:', e);
                  const er = new Error(e);
                  er.message = `graphql-route.get: ${e.message}`;
                  throw er;
                }
              },
            },
          }),
        ],
      },
    ],
  };

  return [deviceGraphqlRoute, deviceRoute, deviceAttrRoute];
};
