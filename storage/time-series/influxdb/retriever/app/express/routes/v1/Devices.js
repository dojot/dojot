const {
  ConfigManager: { getConfig },
  Logger,
  WebUtils: { framework },
} = require('@dojot/microservice-sdk');
const { graphqlHTTP } = require('express-graphql');
const HttpStatus = require('http-status-codes');
const { pipeline, Writable } = require('stream');
const util = require('util');

const pipelineAsync = util.promisify(pipeline);

const AcceptHeaderHelper = require('../../helpers/AcceptHeaderHelper');

const logger = new Logger('influxdb-retriever:express/routes/v1/Devices');

const { graphql: { graphiql } } = getConfig('RETRIEVER');
const rootSchema = require('../../../graphql/Schema');
const DeviceDataService = require('../../services/v1/DeviceDataService');
const { parseCSV } = require('../../helpers/SimpleCSVParser');

const deviceDataSource = {
  memory: 'createStreamInMemory',
  disk: 'createStreamInDisk',
};


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
  deviceManagerService, mountPoint, deviceDataService, genericQueryService, deviceDataRepository,
}) => {
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
  const deviceDataRoute = {
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
                await deviceManagerService.findDevice(req.tenant.id, deviceId);
              } catch (error) {
                logger.debug(error.message);
                throw framework.errorTemplate.NotFound(`Not found ${req.tenant.id}/${deviceId}`);
              }

              const {
                dateFrom, dateTo, limit, page, order,
              } = req.query;

              const [result, paging] = await deviceDataService.getDeviceData(
                req.tenant.id,
                deviceId,
                dateFrom,
                dateTo,
                limit,
                page,
                order,
                req.getPaging,
              );

              res.type(accept);
              if (accept === 'csv') {
                return res.status(HttpStatus.OK).send(
                  DeviceDataService.parseDeviceDataToCsv(result),
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
   * This feature returns devices in cache
   */
  const deviceRoute = {
    mountPoint,
    name: 'devices-route',
    path: ['/devices'],
    handlers: [
      {
        method: 'get',
        middleware: [
          checkDateTo,
          async (req, res) => {
            const devices = [];
            let readStream;
            const { source } = req.params;

            try {
              const writableStream = Writable({
                async write(
                  key, encoding, cb,
                ) {
                  devices.push(key.toString());
                  cb();
                },
              });

              try {
                readStream = await deviceManagerService[deviceDataSource[source]](req.tenant.id);
              } catch (error) {
                readStream = await deviceManagerService.createStreamInDisk(req.tenant.id);
              }

              await pipelineAsync(readStream, writableStream);
            } catch (e) {
              logger.error('devices-route.get:', e);
              return res.status(HttpStatus.INTERNAL_SERVER_ERROR);
            }

            return res.status(HttpStatus.OK).json({ devices });
          },
        ],
      },
    ],
  };

  /**
   * This feature returns data for an attribute with time
   * filter, pagination and order
   */
  const deviceAttrDataRoute = {
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
                await deviceManagerService.findDevice(req.tenant.id, deviceId);
              } catch (error) {
                throw framework.errorTemplate.NotFound(`Not found ${req.tenant.id}/${deviceId}`);
              }

              const {
                dateFrom, dateTo, limit, page, order,
              } = req.query;

              const [result, paging] = await deviceDataService.getDeviceAttrData(
                req.tenant.id,
                deviceId,
                attr,
                dateFrom,
                dateTo,
                limit,
                page,
                order,
                req.getPaging,
              );

              res.type(accept);
              if (accept === 'csv') {
                return res.status(HttpStatus.OK).send(
                  parseCSV(result),
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
                  const res = await deviceDataRepository.queryUsingGraphql(
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

  const queryRoute = {
    mountPoint,
    name: 'query-route',
    path: ['/query'],
    handlers: [
      {
        method: 'post',
        middleware: [
          // eslint-disable-next-line consistent-return
          async (req, res) => {
            try {
              const accept = AcceptHeaderHelper.getAcceptableType(req);

              const { query } = req.body;
              const result = await genericQueryService.runQuery(req.tenant.id, query);

              res.type(accept);
              if (accept === 'csv') {
                return res.status(HttpStatus.OK).send(
                  parseCSV(result),
                );
              }

              res.status(200).json({ result });
            } catch (error) {
              logger.error('query-route', error);
              error.message = `query-route: ${error.message}`;
              throw error;
            }
          },
        ],
      },
    ],
  };

  const flexQueryRoute = {
    mountPoint,
    name: 'flexquery-route',
    path: ['/flexquery'],
    handlers: [
      {
        method: 'post',
        middleware: [
          // eslint-disable-next-line consistent-return
          async (req, res) => {
            try {
              const accept = AcceptHeaderHelper.getAcceptableType(req);

              const { query } = req.body;
              const result = await genericQueryService.runFlexQuery(req.tenant.id, query);

              res.type(accept);
              if (accept === 'csv') {
                return res.status(HttpStatus.OK).send(
                  parseCSV(result),
                );
              }

              res.status(200).json({ result });
            } catch (error) {
              logger.error('flexquery-route', error);
              error.message = `flexquery-route: ${error.message}`;
              throw error;
            }
          },
        ],
      },
    ],
  };

  return [
    deviceGraphqlRoute,
    deviceRoute,
    deviceDataRoute,
    deviceAttrDataRoute,
    queryRoute,
    flexQueryRoute.handlers,
  ];
};
