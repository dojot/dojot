/**
 * A module with helper functions
 * @module utils
 */

const moment = require('moment');

/**
 * @function generateDojotDeviceDataMessage
 *
 * Generates a payload for device-data topic for Dojot
 *
 * @param {string} topic
 * @param {Object} payload
 */
const generateDojotDeviceDataMessage = (topic, payload) => {
  const username = topic.split('/')[0];
  const splitUsername = username.split(':');

  const tenantValue = splitUsername[0];
  const deviceIdValue = splitUsername[1];

  return {
    metadata: {
      deviceid: deviceIdValue,
      tenant: tenantValue,
      timestamp: moment().unix(),
    },
    attrs: payload,
  };
};

const unsecuredMode = (mode) => ((mode || false) && (mode.toString().toLowerCase().trim() === 'true' || Number(mode) > 0));

/**
 * Transforms a string into a boolean (case insensitive).
 *
 * @param {string} value
 */
const toBoolean = (value) => value && (value.toString().toLowerCase() === 'true');

module.exports = { generateDojotDeviceDataMessage, toBoolean, unsecuredMode };
