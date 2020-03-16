/**
 * A module with helper functions
 * @module utils
 */

/**
 * Generates the topic name in which the MQTT client must publish to actuate in a device.
 *
 * @param {string} tenant
 * @param {string} deviceid
 * @param {string} suffix
 */
const generateDojotActuationTopic = (tenant, deviceid, suffix) => `${tenant}:${deviceid}${suffix}`;

module.exports = { generateDojotActuationTopic };
