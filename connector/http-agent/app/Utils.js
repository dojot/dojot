/* eslint-disable security/detect-object-injection */
const createError = require('http-errors');

const err = new createError.BadRequest();

/**
 * A module with helper functions
 * @module utils
 */

/**
 * A module with helper functions
 * @module utils
 */

/**
 * Validates if the attribute is an object
 *
 * @param {Object} obj
 *
 * @returns boolean
 * @private
 */
const isObject = (obj) => Object.prototype.toString.call(obj) === '[object Object]';

/**
   * handles the payload
   *
   * @param {Object} payload
   * @returns Object
   *
   * @private
   */
const attrsHandler = (payload) => {
  const attrs = {};
  Object.entries(payload).forEach(([key, value]) => {
    if (isObject(value)) {
      attrs[key] = JSON.stringify(value);
    } else {
      attrs[key] = value;
    }
  });
  return attrs;
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
  // if wasn't received ts value, return the current timestamp;
  if (!ts) {
    return new Date().getTime();
  }
  if (!isNaN(ts)) {
    const unix = parseInt(ts, 10);
    return new Date(unix).getTime();
  }
  if ((new Date(ts)).getTime() > 0) {
    return new Date(ts).getTime();
  }
  err.message = 'Invalid timestamp';
  throw err;
};

/**
 * @function generateDeviceDataMessage
 *
 * Generates a payload for device-data topic for Dojot
 *
 * @param {object} payload
 * @param {string} tenant
 * @param {string} deviceid
 *
 * @returns {{metadata: {deviceid: string, tenant: string, timestamp: number}, attrs: Object}}
 */
const generateDeviceDataMessage = (payload, tenant, deviceid) => ({
  metadata: {
    deviceid,
    tenant,
    timestamp: parseTimestamp(payload.ts),
  },
  attrs: attrsHandler(payload.data),
});

/**
 * Kills the program process.
 * @function killApplication
 */
const killApplication = () => process.kill(process.pid);

module.exports = {
  generateDeviceDataMessage,
  killApplication,
};
