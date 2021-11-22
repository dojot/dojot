const createError = require('http-errors');

const err = new createError.BadRequest();

/**
 * A module with helper functions
 * @module utils
 */

/**
 * @function isInteger
 *
 * check if is integer
 *
 * @param {string} str
 *
 * @returns {boolean}
 */

const isInteger = (str) => {
  if (typeof str !== 'string') return false; // we only process strings!
  // eslint-disable-next-line max-len
  return !isNaN(str) && // use type coercion to parse the _entirety_ of the string (`parseFloat` alone does not do this)...
         !isNaN(parseInt(str, 10));// ...and ensure strings of whitespace fail
};

/**
 * @function parseTimestamp
 *
 * Return timestamp in proper format
 *
 * @param {string} ts
 *
 * @returns {number}
 */
const parseTimestamp = (ts) => {
  if (ts) {
    if (isInteger(ts)) {
      const isValidDate = (new Date(parseInt(ts, 10))).getTime() > 0;
      if (isValidDate) return new Date(parseInt(ts, 10)).getTime();
    } else {
      const isValidDate = (new Date(ts)).getTime() > 0;
      if (isValidDate) return new Date(ts).getTime();
    }
    err.message = 'Invalid timestamp';
    throw err;
  }
  return new Date().getTime();
};

/**
 * @function generateDeviceDataMessage
 *
 * Generates a payload for device-data topic for Dojot
 *
 * @param {string} payload
 * @param {string} tenant
 * @param {Object} deviceid
 *
 * @returns {{metadata: {deviceid: string, tenant: string, timestamp: number}, attrs: Object}}
 */
const generateDeviceDataMessage = (payload, tenant, deviceid) => {
  const formattedMessage = {
    metadata: {
      deviceid,
      tenant,
      timestamp: parseTimestamp(payload.ts),
    },
    attrs: payload.data,
  };

  return formattedMessage;
};

/**
 * Kills the program process.
 * @function killApplication
 */
const killApplication = () => process.kill(process.pid);

module.exports = {
  generateDeviceDataMessage,
  killApplication,
};
