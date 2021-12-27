const { InfluxDB } = require('@influxdata/influxdb-client');
const { DeleteAPI } = require('@influxdata/influxdb-client-apis');
const { Logger } = require('@dojot/microservice-sdk');

const logger = new Logger('influxdb-storer:influx/Measurement');

/**
 *  This class handles with Measurements in a specific bucket.
 * @class
 */
class Measurements {
  /**
   * Measurement constructor
   *
   * @param {String} url Url to access influxdb
   * @param {String} token A token with deletion permission in all orgs
   * @param {String} defaultBucket The default bucket to be using with all orgs
   */
  constructor(
    url, token, defaultBucket,
  ) {
    logger.debug('constructor:');
    logger.debug(`constructor: url=${url}`);
    logger.debug(`constructor: token=${token}`);
    logger.debug(`constructor: defaultBucket=${defaultBucket}`);
    const influxDB = new InfluxDB({ url, token });
    this.deleteAPI = new DeleteAPI(influxDB);
    this.bucket = defaultBucket;
  }

  /**
   * ATTENTION: At this time when this is call
   * you will receive '501: not implemented'
   *
   * @see {@link https://github.com/influxdata/influxdb-client-js/issues/278}
   *
   * Delete all information from a measurement in a org
   *
   * @param {String} org Organization name
   * @param {String} measurement Measurement name
   */
  async deleteMeasurement(org, measurement) {
    logger.info(`deleteMeasurement: Trying delete ${measurement} measurement in ${org} org...`);
    try {
      await this.deleteAPI.postDelete({
        org,
        bucket: this.bucket,
        body: {
          predicate: `_measurement="${measurement}"`,
          start: '1970-01-01T00:00:00Z',
          stop: new Date().toISOString(),
        },
      });
      logger.info(`deleteMeasurement: The ${measurement} measurement  in ${org} org was delete.`);
    } catch (e) {
      logger.error(`deleteMeasurement: Cannot delete measurement ${measurement} in ${org} org`, e);
      // not throw in deletes
    }
  }
}
module.exports = Measurements;
