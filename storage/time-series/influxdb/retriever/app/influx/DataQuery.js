/* eslint-disable no-underscore-dangle */
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

const logger = new Logger('influxdb-retriever:influx/DataQuery');

/**
 * This class handle with query data in a specific bucket.
 *
 * Note that: We proposed a paging approach using limit and offset, which is not wrong;
 * but it would be more efficient a strategy based on time displacements, i.e,
 * manipulating the dataFrom and dataTo in order to reduce the
 * data set in each iteration.
 *
 * @class
 */
class DataQuery {
  /**
   *
   * @param {String} url   Url to access influxdb
   * @param {String} token  A token with write permission in all orgs
   * @param {String} defaultBucket  Bucket Name for all data write
   * @param {Number} timeout  Request timeout in the communication with the influxdb
   *                          in milliseconds.
   */
  constructor(url, token, defaultBucket, timeout) {
    logger.debug('constructor:');
    logger.debug(`constructor: url=${url}`);
    logger.debug(`constructor: token=${token}`);
    logger.debug(`constructor: defaultBucket=${defaultBucket}`);
    logger.debug(`constructor: timeout=${timeout}`);

    this.influxDB = new InfluxDB({ url, token, timeout });
    this.defaultBucket = defaultBucket;
    // prefix adds to all fields to be written
    this.prefixFields = 'dojot.';
    this.prefixFieldsSize = (this.prefixFields).length;
  }

  /**
 * Fetch data for a given field considering the time
 * slot and paging filter for a default bucket and an given org.
 *
 * @param {String} org Organization Name
 * @param {String} measurement Measurement Name
 * @param {object} filters Filters for query
 * @param {string} filters.dateFrom=1970-01-01T00:00:00.000Z
 * @param {string} filter.dateTo=(current time)
 * @param {object} page Paginate information for query
 * @param {number} page.limit=256
 * @param {number} page.page=1
 * @param {{String='desc','asc'}} order=desc Defines whether the order by **time** should be
 *                                            ascending (asc) or descending (desc)
 *
 * @returns {Promise.<{result: [{ ts: timeIsoData, attrs: [label:string, value:any }],
 *                            totalItems: number}| error>}
 *                            A promise that returns a result e a totalItems
 */
  async queryByMeasurement(org, measurement, filters = {}, page = {}, order = 'desc') {
    try {
      logger.debug('queryByMeasurement:');
      logger.debug(`queryByMeasurement: org=${org}`);
      logger.debug(`queryByMeasurement: measurement=${measurement}`);
      logger.debug(`queryByMeasurement: filters=${util.inspect(filters)}`);
      logger.debug(`queryByMeasurement: page=${util.inspect(page)}`);

      const {
        start, stop, limit, offset,
      } = DataQuery.commonQueryParams(page, filters);

      const orderExp = DataQuery.commonQueryOrderExpression(order);
      const limitExp = DataQuery.commonLimitExpression(limit, offset);

      const fluxQuery = flux`from(bucket:${fluxString(this.defaultBucket)})
      |> range(start: ${start} , stop: ${stop})
      |> filter(fn: (r) => r._measurement == ${fluxString(measurement)})
      |> pivot(rowKey:["_time"], columnKey: ["_field"], valueColumn: "_value")
      ${fluxExpression(orderExp)}
      ${fluxExpression(limitExp)}`;

      logger.debug(`queryByMeasurement: fluxQuery=${fluxQuery}`);

      const queryApi = this.influxDB.getQueryApi({ org, gzip: false });
      const prefix = this.prefixFields;
      const prefixSize = this.prefixFieldsSize;

      return new Promise((resolve, reject) => {
        const result = [];
        queryApi.queryRows(fluxQuery, {
          next(row, tableMeta) {
            const o = tableMeta.toObject(row);
            logger.debug(`queryByMeasurement: queryRows.next=${JSON.stringify(o, null, 2)}`);
            const point = {
              ts: o._time,
              attrs: [],
            };

            delete o._time;
            Object.entries(o).forEach(([key, value]) => {
              // check if has 'dojot.' at begin
              // https://measurethat.net/Benchmarks/Show/5016/1/replace-vs-substring-vs-slice-from-beginning-brackets-s
              if (key.substring(0, prefixSize) === prefix
                // boolean and numbers that do not exist at that point are null
                && value !== null
                // strings that don't exist for that point are empty
                && value !== '') {
                point.attrs.push({
                  label: key.slice(prefixSize),
                  value: JSON.parse(value),
                });
              }
            });
            result.push(point);
          },
          error(error) {
            return reject(DataQuery.commonHandleError(error));
          },
          complete() {
            logger.debug(`queryByMeasurement: result=${JSON.stringify(result, null, 2)} totalItems=${result.length}`);
            return resolve({ result, totalItems: result.length });
          },
        });
      });
    } catch (e) {
      logger.error('queryByMeasurement:', e);
      throw e;
    }
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
 * @returns {Promise.<{result: [{ ts: timeIsoData(string), value: any }],
 *                            totalItems: number}| error>}
 *                            A promise that returns a result e a totalItems
 */
  async queryByField(org, measurement, field, filters = {}, page = {}, order = 'desc') {
    try {
      logger.debug('queryByField:');
      logger.debug(`queryByField: org=${org}`);
      logger.debug(`queryByField: measurement=${measurement}`);
      logger.debug(`queryByField: field=${field}`);
      logger.debug(`queryByField: filters=${util.inspect(filters)}`);
      logger.debug(`queryByField: page=${util.inspect(page)}`);

      const {
        start, stop, limit, offset,
      } = DataQuery.commonQueryParams(page, filters);

      const orderExp = DataQuery.commonQueryOrderExpression(order);
      const limitExp = DataQuery.commonLimitExpression(limit, offset);

      const fluxQuery = flux`from(bucket:${fluxString(this.defaultBucket)})
        |> range(start: ${start} , stop: ${stop})
        |> filter(fn: (r) => r._measurement == ${fluxString(measurement)} and r._field == ${fluxString(`dojot.${field}`)})
        ${fluxExpression(orderExp)}
        ${fluxExpression(limitExp)}`;

      logger.debug(`queryByField: fluxQuery=${fluxQuery}`);

      const queryApi = this.influxDB.getQueryApi({ org, gzip: false });

      return new Promise((resolve, reject) => {
        const result = [];
        queryApi.queryRows(fluxQuery, {
          next(row, tableMeta) {
            const o = tableMeta.toObject(row);
            logger.debug(`queryByField: queryRows.next=${JSON.stringify(o, null, 2)}`);
            // when storer write the data it just check if is a number or a boolean
            // the others types are writer as string with json stringify
            result.push({
              ts: o._time,
              value: JSON.parse(o._value),
            });
          },
          error(error) {
            return reject(DataQuery.commonHandleError(error));
          },
          complete() {
            logger.debug(`queryByField: result=${JSON.stringify(result, null, 2)} totalItems=${result.length}`);
            return resolve({ result, totalItems: result.length });
          },
        });
      });
    } catch (e) {
      logger.error('queryByField:', e);
      throw e;
    }
  }

  /**
   * Handles error coming from the influx lib to be used by the sdk web lib
   * @param {Error} error
   * @returns  {Error}
   */
  static commonHandleError(error) {
    const { message } = error.body ? JSON.parse(error.body) : {};
    const { statusMessage, statusCode } = error;
    let newError = error;
    if (statusMessage && statusCode && message) {
      newError = createError(statusCode, `InfluxDB: ${statusMessage} -> ${message}`);
    }
    return (newError);
  }

  /**
   * Handles query filters that are common
   * @param {object} filters Filters for query
   * @param {string} filters.dateFrom
   * @param {string} filter.dateTo
   * @param {object} page Paginate information for query
   * @param {number} page.limit
   * @param {number} page.page
   *
   * @returns
   */
  static commonQueryParams(page, filters) {
    const limit = page.limit ? fluxInteger(page.limit) : 256;
    const pageNumber = page.page && page.page >= 1 ? fluxInteger(page.page) : 1;
    const start = filters.dateFrom ? fluxDateTime(filters.dateFrom) : 0;
    const stop = filters.dateTo ? fluxDateTime(filters.dateTo) : fluxExpression('now()');
    return {
      start, stop, limit, offset: (pageNumber - 1) * limit,
    };
  }

  /**
   * Handles query order that is common
   *
   * @param {{String='desc','asc'}} order Defines whether the order by **time** should be
   *                                            ascending (asc) or descending (desc)
   * @returns
   */
  static commonQueryOrderExpression(order) {
    let orderExp = '';
    if (order === 'desc') {
      orderExp = '|> sort(columns: ["_time"], desc: true)';
    }
    return orderExp;
  }

  /**
   * Handles query limit that is common
   *
   * @param {number} limit
   * @param {number} offset
   * @returns
   */
  static commonLimitExpression(limit, offset) {
    return `|> limit(n: ${limit} , offset: ${offset})`;
  }
}

module.exports = DataQuery;
