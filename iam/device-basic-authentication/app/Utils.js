const { v4: uuidv4 } = require('uuid');

/**
 * A module with helper functions
 * @module utils
 */

/**
 * @function generateMessage
 *
 * Generates a message payload to notify that credentials were created to a given device
 *
 * @param {string} tenant
 * @param {string} deviceid
 *
 * @returns {{metadata: {deviceid: string, tenant: string, timestamp: number}, attrs: Object}}
 */
const generateMessage = (tenant, device) => ({
  metadata: {
    msgid: uuidv4(),
    ts: new Date().valueOf(),
    service: 'basic-auth',
    content_type: 'application/vnd.dojot.devices.basic-credentials+json',
  },
  data: {
    opr: 'create',
    tenant,
    device,
  },
});

/**
 * Kills the program process.
 * @function killApplication
 */
const killApplication = () => process.kill(process.pid, 'SIGTERM');

module.exports = {
  generateMessage,
  killApplication,
};
