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
  if (ts) {
    const parsedTimestamp = Date.parse(ts);
    if (Number.isNaN(parsedTimestamp)) return new Date().getTime();
    return parsedTimestamp;
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
const generateDeviceDataMessage = (
  payload, tenant, deviceid,
) => {
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
