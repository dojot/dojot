const { InfluxDB, Point } = require('@influxdata/influxdb-client');
const { Logger } = require('@dojot/microservice-sdk');
const { parseDateTimeToUnixNs } = require('../Utils');

const logger = new Logger('influxdb-storer:influxdb/WriterData');
/**
 * This class handle with writer data in a specific bucket.
 * @class
 */
class Writer {
  /**
   *
   * @param {String} url   Url to access influxdb
   * @param {String} token  A token with write permission in all orgs
   * @param {String} defaultBucket  Bucket Name for all data write
   * @param {Object} writeOptions  Default {} (See more at https://influxdata.github.io/influxdb-client-js/influxdb-client.writeoptions.html)
   *
   * @throws If the value for `writeOptions.flushInterval` must be greater than 0
   */
  constructor(url, token, defaultBucket, writeOptions = {}) {
    logger.debug('constructor:');
    logger.debug(`constructor: url=${url}`);
    logger.debug(`constructor: token=${token}`);
    logger.debug(`constructor: defaultBucket=${defaultBucket}`);
    logger.debug('constructor: writeOptions=', writeOptions);
    this.bucket = defaultBucket;
    this.precision = 'ns';
    this.writeOptions = writeOptions;

    if (this.writeOptions.flushInterval && this.writeOptions.flushInterval <= 0) {
      throw new Error('The value for `writeOptions.flushInterval` must be greater than 0');
    }

    this.writeOptions.writeFailed = (error, lines, attempts) => {
      logger.error(`writeFailed: lines: ${lines.toString()} attempts:${attempts}`, error);
    };

    this.writeOptions.writeSuccess = (lines) => {
      logger.debug(`writeSuccess: lines: ${lines.toString()}`);
    };

    this.influxDB = new InfluxDB({ url, token });

    /**
     * Map instances for WriteApi for each org
     */
    this.writers = new Map();
  }

  /**
   * Get an instance of  WriteApi for the supplied organization and bucket.
   *
   * @param {string} org Organization Name
   *
   * @throws If Cannot get Writer for a org
   */
  getWriter(org) {
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
   * as the messages are sent in batch there is no way to be sure which message was write
   * to commit or not, even changing the batch to 1 and deactivating the auto flush,
   * something that can be used to confirm the data is not returned.
   * writeSuccess does not currently appear to be called and there would be a cost
   * to parse the data that comes in rows. https://github.com/influxdata/influxdb-client-js/issues/279
   */
  async writerData(org, measurement, attrs, timestamp) {
    logger.debug(`writer: Pushing  data to ${org} org, ${measurement} measu and ${timestamp} timestamp and attrs`);
    if (typeof attrs === 'object') {
      try {
        const point = new Point(measurement);
        point.timestamp(Writer.parseDataToInfluxDB(timestamp));
        Object.entries(attrs).forEach(([key, value]) => {
          logger.debug(`writer: setting key=${key}, value=${value}, type=${typeof value}`);
          if (typeof value === 'number') {
            point.floatField(key, value);
          } else if (typeof value === 'boolean') {
            point.booleanField(key, value);
          } else {
            point.stringField(key, JSON.stringify(value));
          }
        });
        logger.debug(`writer: The point will be write is ${point.toString()} in ${org} org`);
        this.getWriter(org).writePoint(point);
      } catch (e) {
        logger.error('writer:', e);
        throw new Error('Cannot write data');
      }
    } else {
      logger.warn('The attrs param is not a object');
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
        this.writers.delete(org);
        await this.writers.get(org).close();
        logger.warn(`closeOne: The ${org} orgs was closed`);
      } else {
        logger.warn(`closeOne: The ${org} orgs doest exist to Write yet`);
      }
    } catch (e) {
      logger.error('closeOne:', e);
    }
  }
}

module.exports = Writer;
