
/**
 * A module with helper functions
 * @module utils
 */

/**
 * @function generateMessage
 *
 * Generates a payload for device-data topic for Dojot
 *
 * @param {string} tenant
 * @param {Object} deviceid
 *
 * @returns {{metadata: {deviceid: string, tenant: string, timestamp: number}, attrs: Object}}
 */
const generateMessage = (event, tenant, deviceid) => {
  const message = {
    event,
    meta: {
      service: tenant,
    },
    data: {
      id: deviceid,
    },
  };

  return message;
};

/**
 * Kills the program process.
 * @function killApplication
 */
const killApplication = () => process.kill(process.pid, 'SIGTERM');

module.exports = {
  generateMessage,
  killApplication,
};
