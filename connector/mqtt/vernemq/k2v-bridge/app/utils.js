/**
 * A module with helper functions
 * @module utils
 */

/**
 * Generates the topic name in which the MQTT client must publish to actuate in a device.
 * @function generateDojotActuationTopic
 *
 * @param {string} tenant
 * @param {string} deviceId
 * @param {string} suffix
 *
 * @returns {string}
 */
const generateDojotActuationTopic = (tenant, deviceId, suffix) => `${tenant}:${deviceId}${suffix}`;

/**
 * Transforms a string into a boolean (case insensitive).
 * @function toBoolean
 *
 * @param {string} value
 *
 * @returns {boolean}
 */
const toBoolean = (value) => (value && (value.toString().toLowerCase().trim() === 'true'));

/**
 * Kills the program process.
 * @function killApplication
 */
const killApplication = () => process.kill(process.pid);

module.exports = { generateDojotActuationTopic, toBoolean, killApplication };
