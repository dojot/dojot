const createError = require('http-errors');

const err = new createError.BadRequest();

/**
 * A module with helper functions
 * @module utils
 */

/**
 * @function sslCADecode
 *
 * Return certificates separated by line breaks
 *
 * @param {string} source
 *
 * @returns {string}
 */
const sslCADecode = (source) => {
  if (!source || typeof source !== 'string') {
    return [];
  }

  const sourceArray = source
    .split('-----END CERTIFICATE-----\n-----BEGIN CERTIFICATE-----')
    .map((value, index, array) => {
      let certificate = value;
      if (index) {
        certificate = `-----BEGIN CERTIFICATE-----${value}`;
      }
      if (index !== array.length - 1) {
        certificate += '-----END CERTIFICATE-----';
      }
      certificate = certificate.replace(/^\n+/, '').replace(/\n+$/, '');
      return certificate;
    });

  let allCAs = '';

  sourceArray.forEach((caSource) => {
    allCAs += `${caSource}\n`;
  });
  return allCAs;
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
  attrs: payload.data,
});

/**
 * Kills the program process.
 * @function killApplication
 */
const killApplication = () => process.kill(process.pid);

module.exports = {
  sslCADecode,
  generateDeviceDataMessage,
  killApplication,
};
