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
 */
const generateDojotActuationTopic = (tenant, deviceId, suffix) => `${tenant}:${deviceId}${suffix}`;

/**
 * Transforms a string into a boolean (case insensitive).
 * @function toBoolean
 *
 * @param {string} value
 */
const toBoolean = (value) => (value && (value.toString().toLowerCase().trim() === 'true'));

module.exports = { generateDojotActuationTopic, toBoolean };
