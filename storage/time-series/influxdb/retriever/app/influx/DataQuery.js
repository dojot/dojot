const createError = require('http-errors');
const {
  InfluxDB,
  flux,
  fluxExpression,
  fluxInteger,
  fluxDateTime,
  fluxString,
} = require('@influxdata/influxdb-client');
const util = require('util');
const { Logger } = require('@dojot/microservice-sdk');

const logger = new Logger('influxdb-retriever:influx/QueryData');

/**
 * This class handle with query data in a specific bucket.
 * @class
 */
class DataQuery {
  /**
   *
   * @param {String} url   Url to access influxdb
   * @param {String} token  A token with write permission in all orgs
   * @param {String} defaultBucket  Bucket Name for all data write
   *
   */
  constructor(url, token, defaultBucket) {
    logger.debug('constructor:');
    logger.debug(`constructor: url=${url}`);
    logger.debug(`constructor: token=${token}`);
    logger.debug(`constructor: defaultBucket=${defaultBucket}`);

    this.influxDB = new InfluxDB({ url, token });
    this.defaultBucket = defaultBucket;
  }


  /**
 * Fetch data for a given field considering the time
 * slot and paging filter for a default bucket and an given org.
 *
 * @param {String} org Organization Name
 * @param {String} measurement Measurement Name
 * @param {String} field attr name
 * @param {object} filters Filters for query
 * @param {string} filters.dateFrom=1970-01-01T00:00:00.000Z
 * @param {string} filter.dateTo=(current time)
 * @param {object} page Paginate information for query
 * @param {number} page.limit=256
 * @param {number} page.page=1
 * @param {{String='desc','asc'}} order=desc Defines whether the order by **time** should be
 *                                            ascending (asc) or descending (desc)
 *
 * @returns {Promise.<{result: { timeIsoData(string): value (*) }, totalItems: number}| error>}
 *                            A promise that returns a result e a totalItems
 */
  async queryByField(org, measurement, field, filters = {}, page = {}, order = 'desc') {
    try {
      logger.debug('queryData:');
      logger.debug(`queryData: org=${org}`);
      logger.debug(`queryData: measurement=${measurement}`);
      logger.debug(`queryData: field=${field}`);
      logger.debug(`queryData: filters=${util.inspect(filters)}`);
      logger.debug(`queryData: page=${util.inspect(page)}`);


      const limit = page.limit ? fluxInteger(page.limit) : 256;
      const offset = page.page && page.page >= 1 ? fluxInteger(page.page - 1) : 0;
      const start = filters.dateFrom ? fluxDateTime(filters.dateFrom) : 0;
      const stop = filters.dateTo ? fluxDateTime(filters.dateTo) : fluxExpression('now()');

      let orderExp = '';
      if (order === 'desc') {
        orderExp = '|> sort(columns: ["_time"], desc: true)';
      }

      const fluxQuery = flux`from(bucket:${fluxString(this.defaultBucket)})
        |> range(start: ${start} , stop: ${stop})
        |> filter(fn: (r) => r._measurement == ${fluxString(measurement)} and r._field == ${fluxString(field)})
        |> drop(columns: ["_start", "_stop", "_measurement"])
        ${fluxExpression(orderExp)}
        |> limit(n: ${limit} , offset: ${offset})`;

      logger.debug(`queryData: fluxQuery=${fluxQuery}`);

      const queryApi = this.influxDB.getQueryApi({ org, gzip: false });

      return new Promise((resolve, reject) => {
        const result = [];
        queryApi.queryRows(fluxQuery, {
          next(row, tableMeta) {
            const o = tableMeta.toObject(row);
            // when storer write the data it just check if is a number or a boolean
            // the others types are writer as string with json stringify
            result.push({
              // eslint-disable-next-line no-underscore-dangle
              ts: o._time,
              value: typeof value === 'number' || typeof value === 'boolean'
              // eslint-disable-next-line no-underscore-dangle
                ? o._value : JSON.parse(o._value),
            });
          },
          error(error) {
            const { message } = error.body ? JSON.parse(error.body) : {};
            const { statusMessage, statusCode } = error;
            if (statusMessage && statusCode && message) {
              return reject(createError(statusCode, `InfluxDB: ${statusMessage} -> ${message}`));
            }
            return reject(error);
          },
          complete() {
            logger.debug(`queryData: totalItems=${result.length} result=${util.inspect(result)}`);
            return resolve({ result, totalItems: result.length });
          },
        });
      });
    } catch (e) {
      logger.error('queryData:', e);
      throw e;
    }
  }
}

module.exports = DataQuery;
