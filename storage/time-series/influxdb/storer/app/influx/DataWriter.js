const { InfluxDB, Point } = require('@influxdata/influxdb-client');
const { Logger } = require('@dojot/microservice-sdk');
const util = require('util');

const { parseDateTimeToUnixNs } = require('../Utils');

const logger = new Logger('influxdb-storer:influxdb/WriterData');
/**
 * This class handles with writer data in a specific bucket.
 * @class
 */
class DataWriter {
  /**
   *
   * @param {String} url   Url to access influxdb
   * @param {String} token  A token with write permission in all orgs
   * @param {Number} timeout Connection timeout with influxdb
   * @param {String} defaultBucket  Bucket Name for all data write
   * @param {Boolean} writeAsString
   * @param {Object} writeOptions  Default {} (See more at https://influxdata.github.io/influxdb-client-js/influxdb-client.writeoptions.html)
   *
   * @throws If the value for `writeOptions.flushInterval` must be greater than 0
   */
  constructor(url, token, timeout, defaultBucket, writeAsString, writeOptions = {}) {
    logger.debug('constructor:');
    logger.debug(`constructor: url=${url}`);
    logger.debug(`constructor: token=${token}`);
    logger.debug(`constructor: timeout=${timeout}`);
    logger.debug(`constructor: defaultBucket=${defaultBucket}`);
    logger.debug(`constructor: writeAsString=${writeAsString}`);
    logger.debug(`constructor: writeOptions=${JSON.stringify(writeOptions)}`);
    this.bucket = defaultBucket;
    this.precision = 'ns';
    this.writeOptions = writeOptions;
    this.writeAsString = writeAsString;

    if (this.writeOptions.flushInterval && this.writeOptions.flushInterval <= 0) {
      throw new Error('The value for `writeOptions.flushInterval` must be greater than 0');
    }

    //  WriteFailed is called to inform about write errors.
    //  this - the instance of the API that failed
    //  error - write error
    //  lines - failed lines
    //  attempts - a number of failed attempts to write the lines
    //  returns a Promise to force the API to use it as a result of the flush operation,
    //  void/undefined to continue with default retry mechanism
    this.writeOptions.writeFailed = (error, lines, attempts) => {
      logger.debug(`writeFailed: lines: ${lines.toString()} attempts:${attempts}`, error);
    };

    // WriteSuccess is informed about successfully written lines.
    // this - the instance of the API in use
    // lines: Array<string>
    this.writeOptions.writeSuccess = (lines) => {
      logger.debug(`writeSuccess: lines: ${lines.toString()}`);
    };

    logger.debug(`final writeOptions: ${util.inspect(writeOptions)}`);

    this.influxDB = new InfluxDB({ url, token, timeout });

    /**
     * Map instances for WriteApi for each org
     */
    this.writers = new Map();

    // prefix adds to all fields to be written
    this.prefixFields = 'dojot.';
  }

  /**
   * Gets an instance of  WriteApi for the supplied organization and bucket.
   *
   * @param {string} org Organization Name
   *
   * @throws If Cannot get Writer for a org
   */
  getWriteAPI(org) {
    logger.debug(`getWriter:  Getting witter for ${org} org..`);
    try {
      if (!this.writers.has(org)) {
        this.writers.set(
          org,
          this.influxDB.getWriteApi(
            org,
            this.bucket,
            this.precision,
            this.writeOptions,
          ),
        );
      }
      return this.writers.get(org);
    } catch (e) {
      logger.error('getWriter:', e);
      throw new Error('Cannot get Writer for a org');
    }
  }


  /**
   * Write data in a org and measurement
   *
   * @param {String} org   Organization Name
   * @param {String} measurement   Measurement Name
   * @param {object} attrs A object with key-value where value cold be any json valid type
   * @param {number|string} timestamp A integer  unix timestamp ms
   *                        or a string restricted ISO 8601 (YYYY-MM-DDThh:mm:ss.fffffffffZ)
   *                        being fffffffff optional
   * @throws If Cannot write data
   *
   * NOTE: in the future, the strategy must change to ensure that a message is not lost,
   * the commit must be manual for kafka for example, but here a batch strategy from the library.
   * When something goes wrong when writing, an exception is not thrown and
   * as the messages are sent in batch there is no way to be sure which message was written
   * to commit or not, even changing the batch to 1 and deactivating the auto flush,
   * something that can be used to confirm the data is not returned.
   * WriteSuccess and writeFailed could be used for this.
   */
  async write(org, measurement, attrs, timestamp) {
    logger.debug(`writer: Pushing  data to ${org} org, ${measurement} measurement and ${timestamp} timestamp and attrs=${JSON.stringify(attrs)}`);
    if (typeof attrs === 'object') {
      try {
        const point = new Point(measurement);
        point.timestamp(DataWriter.parseDataToInfluxDB(timestamp));
        Object.entries(attrs).forEach(([key, value]) => {
          const newKey = `${this.prefixFields}${key}`;
          logger.debug(`writer: setting key=${newKey}, value=${value}, type=${typeof value}`);
          if (this.writeAsString) {
            point.stringField(newKey, JSON.stringify(value));
          } else if (typeof value === 'number') {
            point.floatField(newKey, value);
          } else if (typeof value === 'boolean') {
            point.booleanField(newKey, value);
          } else {
            point.stringField(newKey, value);
          }
        });
        logger.debug(`writer: The point will be write is ${point.toString()} in ${org} org`);
        this.getWriteAPI(org).writePoint(point);
      } catch (e) {
        logger.error('writer:', e);
        throw new Error('Cannot write data');
      }
    } else {
      logger.warn('writer: The attrs param is not a object');
    }
  }

  /**
   * Parse data to use with influxdb writer
   *
   * @param {number|string} timestamp A integer unix timestamp ms
   *                        or a string restricted ISO 8601 (YYYY-MM-DDThh:mm:ss.fffffffffZ)
   * @returns a string representing the nanoseconds or empty to using current nanoseconds
   */
  static parseDataToInfluxDB(timestamp) {
    try {
      if (timestamp && Number.isInteger(timestamp) && timestamp >= 0) {
        return `${timestamp}000000`;
      } if (timestamp && typeof timestamp === 'string') {
        return parseDateTimeToUnixNs(timestamp);
      }
    } catch (e) {
      logger.warn(`parseDataToInfluxDB: Some error when trying parse the timestamp ${timestamp} `, e);
    }

    logger.warn(`parseDataToInfluxDB: received some invalid timestamp ${timestamp}, will use the current from service`);
    // An empty string can be used to let the server assign the timestamp.
    return '';
  }

  /**
   * Close all WriterApi to flush something that remain
   *
   */
  async closeAll() {
    try {
      const promises = [];
      this.writers.forEach((value) => {
        promises.push(value.close());
      });
      await Promise.all(promises);
      logger.debug('closeAll: The all orgs were closed');
    } catch (e) {
      logger.error('closeAll:', e);
    }
  }

  /**
   * Close a WriterApi org to flush something that remain
   *
   */
  async closeOne(org) {
    try {
      if (this.writers.has(org)) {
        await this.writers.get(org).close();
        this.writers.delete(org);
        logger.warn(`closeOne: The ${org} orgs was closed`);
      } else {
        logger.warn(`closeOne: The ${org} orgs doest exist to Write yet`);
      }
    } catch (e) {
      logger.error('closeOne:', e);
    }
  }
}

module.exports = DataWriter;
