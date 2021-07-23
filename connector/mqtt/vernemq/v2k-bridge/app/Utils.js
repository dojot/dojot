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
  let metadata = { timestamp: 0 };

  if ("timestamp" in payload) {
    // metadata = { timestamp: 0 };
    // If it is a number, just copy it. Probably Unix time.
    if (typeof payload.timestamp === "number") {
      if (!Number.isNaN(payload.timestamp)) {
        metadata.timestamp = payload.timestamp;
      } else {
        this.logger.info('Received an invalid timestamp (NaN)');
        metadata = {};
      }
    } else {
      // If it is a ISO string...
      const parsed = Date.parse(payload.timestamp);
      if (!Number.isNaN(parsed)) {
        metadata.timestamp = parsed;
      } else {
        // Invalid timestamp.
        metadata = {};
      }
    }
  } else {
    metadata.timestamp = Date.now();
  }

  return {
    metadata: {
      deviceid: deviceIdValue,
      tenant: tenantValue,
      timestamp: metadata.timestamp,
    },
    attrs: payload,
  };
};

/**
 * Transforms a string into a boolean (case insensitive).
 *
 * @param {string} value
 */
const toBoolean = (value) => value && (value.toString().toLowerCase().trim() === 'true');

module.exports = { generateDojotDeviceDataMessage, toBoolean };
