/**
 * A module contains helpers functions
 * @module utils
 */

const moment = require('moment');

/**
 * @function generateDojotDeviceDataMessage
 *
 * Generate a payload for device Data topic for Dojot
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

module.exports = { generateDojotDeviceDataMessage };
