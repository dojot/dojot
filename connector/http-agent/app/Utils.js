const moment = require('moment');
const createError = require('http-errors');

/**
 * A module with helper functions
 * @module utils
 */

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
  const err = new createError.BadRequest();
  const date = moment(ts);
  const unix = moment(ts, 'X', true);
  if (date.isValid()) return date.valueOf();
  if (unix.isValid()) return unix.valueOf();
  err.message = 'Invalid timestamp';
  throw err;
};

/**
 * @function generateDeviceDataMessage
 *
 * Generates a payload for device-data topic for Dojot
 *
 * @param {Object} payload
 * @param {string} tenant
 * @param {string} deviceid
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
