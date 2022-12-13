const { Logger } = require('@dojot/microservice-sdk');

const logger = new Logger('v2k-bridge');

/**
 * A module with helper functions
 * @module utils
 */

/**
 * @function generateDojotDeviceDataMessage
 *
 * Generates a payload for device-data topic for Dojot
 *
 * @param {string} topic
 * @param {Object} payload
 *
 * @returns {{metadata: {deviceid: string, tenant: string, timestamp: number}, attrs: Object}}
 */
const generateDojotDeviceDataMessage = (topic, payload) => {
  const username = topic.split('/')[0];
  const splitUsername = username.split(':');

  const tenantValue = splitUsername[0];
  const deviceIdValue = splitUsername[1];

  const attrs = payload;

  let timestamp = Date.now();

  const overwriteTimestamp = (ts) => {
    if (!Number.isNaN(ts)) {
      timestamp = ts;
    } else {
      logger.warn(`Timestamp ${payload.timestamp} is invalid. `
        + 'It\'ll be considered the current time.');
    }
  };

  if (Object.prototype.hasOwnProperty.call(payload, 'timestamp')) {
    // If it is a number, just copy it. Probably Unix time.
    if (typeof payload.timestamp === 'number') {
      overwriteTimestamp(payload.timestamp);
    } else if (typeof payload.timestamp === 'string') {
      // If it is a ISO string...
      overwriteTimestamp(Date.parse(payload.timestamp));
    } else {
      logger.warn(`Times+tamp ${payload.timestamp} is invalid. `
      + 'It\'ll be considered the current time.');
    }
    delete attrs.payload;
  }

  return {
    metadata: {
      tenant: tenantValue,
      deviceid: deviceIdValue,
      timestamp,
    },
    attrs,
  };
};

/**
 * Transforms a string into a boolean (case insensitive).
 *
 * @param {string} value
 */
const toBoolean = (value) => value && (value.toString().toLowerCase().trim() === 'true');

/**
 * Kills the program process.
 * @function killApplication
 */
const killApplication = () => process.kill(process.pid);

module.exports = {
  generateDojotDeviceDataMessage, toBoolean, killApplication,
};
