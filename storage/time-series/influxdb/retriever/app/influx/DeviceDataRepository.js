/* eslint-disable no-underscore-dangle */
const createError = require('http-errors');
const {
  flux,
  fluxExpression,
  fluxInteger,
  fluxDateTime,
  fluxString,
  fluxDuration,
} = require('@influxdata/influxdb-client');
const util = require('util');

const { AuthorizationsAPI, OrgsAPI, BucketsAPI } = require('@influxdata/influxdb-client-apis');

const { InfluxDB } = require('@influxdata/influxdb-client');

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
class DeviceDataRepository {
  /**
   *
   * @param {String} defaultBucket  Bucket Name for all data write
   * @param {@influxdata/influxdb-client/InfluxDB} influxDBConnection  Request timeout in
   *  the communication with the influxdb
   * @param {Boolean} readAsString Indicates whether to read Influxdb data as a String
   *
   */
  constructor(defaultBucket, influxDBConnection, logger) {
    logger.debug('constructor:');
    this.logger = logger;

    this.influxDB = influxDBConnection;
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
      this.logger.debug('queryByMeasurement:');
      this.logger.debug(`queryByMeasurement: org=${org}`);
      this.logger.debug(`queryByMeasurement: measurement=${measurement}`);
      this.logger.debug(`queryByMeasurement: filters=${util.inspect(filters)}`);
      this.logger.debug(`queryByMeasurement: page=${util.inspect(page)}`);

      const {
        start, stop, limit, offset,
      } = DeviceDataRepository.commonQueryParams(page, filters);

      const orderExp = DeviceDataRepository.commonQueryOrderExpression(order);
      const limitExp = DeviceDataRepository.commonLimitExpression(limit, offset);

      const fluxQuery = flux`from(bucket:${fluxString(this.defaultBucket)})
      |> range(start: ${start} , stop: ${stop})
      |> filter(fn: (r) => r._measurement == ${fluxString(measurement)})
      |> pivot(rowKey:["_time"], columnKey: ["_field"], valueColumn: "_value")
      ${fluxExpression(orderExp)}
      ${fluxExpression(limitExp)}`;

      this.logger.debug(`queryByMeasurement: fluxQuery=${fluxQuery}`);

      const queryApi = this.influxDB.getQueryApi({ org, gzip: false });
      const prefix = this.prefixFields;
      const prefixSize = this.prefixFieldsSize;
      const loggerOuter = this.logger;

      return new Promise((resolve, reject) => {
        const result = [];
        queryApi.queryRows(fluxQuery, {
          next(row, tableMeta) {
            const o = tableMeta.toObject(row);
            loggerOuter.debug(`queryByMeasurement: queryRows.next=${JSON.stringify(o, null, 2)}`);
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
                  value,
                });
              }
            });
            result.push(point);
          },
          error(error) {
            return reject(DeviceDataRepository.commonHandleError(error));
          },
          complete() {
            loggerOuter.debug(`queryByMeasurement: result=${JSON.stringify(result, null, 2)} totalItems=${result.length}`);
            return resolve({ result, totalItems: result.length });
          },
        });
      });
    } catch (e) {
      this.logger.error('queryByMeasurement:', e);
      throw e;
    }
  }


  /**
 * Fetch data after using a GraphQL query following the params below
 *
 * @param {String} org Organization Name (Dojot's Tenant)
 * @param {Array} devices a list of devices containing
 * device id and its attributes list
 * @param {object} filters Filters for query
 * @param {string} filters.dateFrom earliest time to include in results
 *                 (-1h, 2019-08-28T22:00:00Z, or 1567029600.)
 * @param {string} filters.dateFrom latest time to include in results
 *                 (-1h, 2019-08-28T22:00:00Z, 1567029600, or current_time.)
 * @param {object} page Paginate information for query
 * @param {number} page.limit= number of results to be returned
 * @param {{String='desc','asc'}} order=desc Defines whether the order
 * by **time** should be ascending (asc) or descending (desc)
 *
 * @returns {Promise.<{result: [{ ts: timeIsoData(string),
 *                                value: any,
 *                                id: string,
 *                                attr: string }]}| error>}
 *           A promise that returns a list of points from influxDB
 */
  async queryUsingGraphql(org, devices, filters = {}, page = {}, order = 'desc') {
    try {
      this.logger.debug('queryUsingGraphql: Handling query created using Graphql.');
      this.logger.debug(`queryUsingGraphql: org=${org}`);
      this.logger.debug(`queryUsingGraphql: devices=${util.inspect(devices)}`);
      this.logger.debug(`queryUsingGraphql: filters=${util.inspect(filters)}`);
      this.logger.debug(`queryUsingGraphql: page=${util.inspect(page)}`);
      this.logger.debug(`queryUsingGraphql: order=${order}`);

      const {
        start, stop, limit, offset,
      } = DeviceDataRepository.commonQueryParams(page, filters);

      const orderExp = DeviceDataRepository.commonQueryOrderExpression(order);
      const limitExp = DeviceDataRepository.commonLimitExpression(limit, offset);

      const fluxQuery = `from(bucket:${fluxString(this.defaultBucket)})
        |> range(start: ${start} , stop: ${stop})
        ${DeviceDataRepository.createFluxFilter(devices)}
        ${fluxExpression(orderExp)}
        ${fluxExpression(limitExp)}`;

      this.logger.debug(`queryByField: fluxQuery=${fluxQuery}`);

      const queryApi = this.influxDB.getQueryApi({ org, gzip: false });

      const loggerOuter = this.logger;

      return new Promise((resolve, reject) => {
        const result = [];
        queryApi.queryRows(fluxQuery, {
          next(row, tableMeta) {
            const o = tableMeta.toObject(row);
            loggerOuter.debug(`queryUsingGraphql: queryRows.next=${JSON.stringify(o, null, 2)}`);
            result.push({
              id: o._measurement,
              ts: o._time,
              value: o._value,
              attr: o._field.replace('dojot.', ''),
            });
          },
          error(error) {
            return reject(DeviceDataRepository.commonHandleError(error));
          },
          complete() {
            loggerOuter.debug(`queryUsingGraphql: result=${JSON.stringify(result, null, 2)} totalItems=${result.length}`);
            return resolve({ data: result });
          },
        });
      });
    } catch (e) {
      this.logger.error('queryUsingGraphql:', e);
      const er = new Error(e);
      er.message = `queryUsingGraphql: ${e.message}`;
      throw er;
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
      this.logger.debug('queryByField:');
      this.logger.debug(`queryByField: org=${org}`);
      this.logger.debug(`queryByField: measurement=${measurement}`);
      this.logger.debug(`queryByField: field=${field}`);
      this.logger.debug(`queryByField: filters=${util.inspect(filters)}`);
      this.logger.debug(`queryByField: page=${util.inspect(page)}`);

      const {
        start, stop, limit, offset,
      } = DeviceDataRepository.commonQueryParams(page, filters);

      const orderExp = DeviceDataRepository.commonQueryOrderExpression(order);
      const limitExp = DeviceDataRepository.commonLimitExpression(limit, offset);

      const fluxQuery = flux`from(bucket:${fluxString(this.defaultBucket)})
        |> range(start: ${start} , stop: ${stop})
        |> filter(fn: (r) => r._measurement == ${fluxString(measurement)} and r._field == ${fluxString(`dojot.${field}`)})
        ${fluxExpression(orderExp)}
        ${fluxExpression(limitExp)}`;

      this.logger.debug(`queryByField: fluxQuery=${fluxQuery}`);

      const queryApi = this.influxDB.getQueryApi({ org, gzip: false });
      const { readAsString } = this;

      const loggerOuter = this.logger;

      return new Promise((resolve, reject) => {
        const result = [];
        queryApi.queryRows(fluxQuery, {
          next(row, tableMeta) {
            const o = tableMeta.toObject(row);
            loggerOuter.debug(`queryByField: queryRows.next=${JSON.stringify(o, null, 2)}`);
            let validatedValue;
            if (readAsString) {
              validatedValue = JSON.parse(o._value);
            } else {
              validatedValue = o._value;
            }
            // when storer write the data it just check if is a number or a boolean
            // the others types are writer as string with json stringify
            result.push({
              ts: o._time,
              value: validatedValue,
            });
          },
          error(error) {
            return reject(DeviceDataRepository.commonHandleError(error));
          },
          complete() {
            loggerOuter.debug(`queryByField: result=${JSON.stringify(result, null, 2)} totalItems=${result.length}`);
            return resolve({ result, totalItems: result.length });
          },
        });
      });
    } catch (e) {
      this.logger.error('queryByField:', e);
      throw e;
    }
  }

  async runGenericQuery(org, query) {
    try {
      let fluxQuery = flux`from(bucket:${fluxString(this.defaultBucket)}) \n |>`;
      fluxQuery += query;

      const queryApi = this.influxDB.getQueryApi({ org, gzip: false });

      const loggerOuter = this.logger;

      return new Promise((resolve, reject) => {
        const result = [];
        queryApi.queryRows(fluxQuery, {
          next(row, tableMeta) {
            const o = tableMeta.toObject(row);
            loggerOuter.debug(`GenericQuery: queryRows.next=${JSON.stringify(o, null, 2)}`);
            result.push(o);
          },
          error(error) {
            return reject(DeviceDataRepository.commonHandleError(error));
          },
          complete() {
            loggerOuter.debug(`queryByField: result=${JSON.stringify(result, null, 2)}`);
            return resolve(result);
          },
        });
      });
    } catch (e) {
      this.logger.error('GenericQuery:', e);
      throw e;
    }
  }

  async runGenericFlexQuery(org, query) {
    try {
      const orgsResponse = await new OrgsAPI(this.influxDB).getOrgs({ org });
      const orgID = orgsResponse.orgs[0].id;
      this.logger.debug(' ', org, orgID);

      const authorizationAPI = new AuthorizationsAPI(this.influxDB);
      const authorizations = await authorizationAPI.getAuthorizations({ orgID });

      let userOrgToken = null;

      if (authorizations.authorizations.length > 0) {
        userOrgToken = authorizations.authorizations[0].token;
      } else {
        this.logger.debug('*** CreateAuthorization ***');

        const name = this.defaultBucket;
        const bucketsAPI = new BucketsAPI(this.influxDB);
        const buckets = await bucketsAPI.getBuckets({ orgID, name });
        const bucketID = buckets.buckets[0].id;

        const auth = await authorizationAPI.postAuthorizations(
          {
            body: {
              description: 'user organization token',
              orgID,
              permissions: [
                {
                  action: 'read',
                  resource: { type: 'buckets', id: bucketID, orgID },
                },
              ],
            },
          },
        );
        userOrgToken = auth.token;
        this.logger.debug(' ', auth.description);
      }

      const userOrgInfluxDBConnection = new InfluxDB({
        url: this.influxDB._options.url,
        token: userOrgToken,
        timeout: this.influxDB._options.timeout,
      });

      const queryApi = userOrgInfluxDBConnection.getQueryApi({ org, gzip: false });

      const loggerOuter = this.logger;

      return new Promise((resolve, reject) => {
        const result = [];
        queryApi.queryRows(query, {
          next(row, tableMeta) {
            const o = tableMeta.toObject(row);
            loggerOuter.debug(`GenericQuery: queryRows.next=${JSON.stringify(o, null, 2)}`);
            result.push(o);
          },
          error(error) {
            return reject(DeviceDataRepository.commonHandleError(error));
          },
          complete() {
            loggerOuter.debug(`queryByField: result=${JSON.stringify(result, null, 2)}`);
            return resolve(result);
          },
        });
      });
    } catch (e) {
      this.logger.error('GenericQuery:', e);
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
    /*  Start is the earliest time to include in results.
        Use a relative duration, absolute time, or integer (Unix
        timestamp in seconds). For example, -1h, 2019-08-28T22:00:00Z,
        or 1567029600.
    */

    const re = new RegExp('^(-)([0-9]+)(w|d|h|(m)(s)?|s)$');
    const isRelative = (str) => re.exec(str);
    let start = 0;
    let stop = fluxExpression('now()');
    if (filters.dateFrom) {
      if (isRelative(filters.dateFrom)) {
        start = fluxDuration(filters.dateFrom);
      } else { start = fluxDateTime(filters.dateFrom); }
    }
    /* Stop is the latest time to include in results and follows the same
       previous pattern. */
    if (filters.dateTo) {
      if (isRelative(filters.dateTo)) {
        stop = fluxDuration(filters.dateTo);
      } else { stop = fluxDateTime(filters.dateTo); }
    }

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

  /**
   * It should be used to create the influx query filter,
   * passing a device list
   *
   * @param {object} devices device list [{id=String, attributes[String]}]
   * @returns String
   */
  static createFluxFilter(devices) {
    let rtnString = '|> filter(fn: (r) => ';
    const strDevices = devices.map((device) => {
      let rtnDev = `(r._measurement == ${fluxString(device.id)}`;
      const strAttrs = device.attributes.map((attr) => `r._field == ${fluxString(`dojot.${attr}`)}`).join(' or ');

      if (device.attributes.length) { rtnDev += ` and (${strAttrs})`; }
      rtnDev += ')';
      return rtnDev;
    }).join(' or ');
    rtnString += `${strDevices})`;
    return rtnString;
  }
}

module.exports = DeviceDataRepository;
