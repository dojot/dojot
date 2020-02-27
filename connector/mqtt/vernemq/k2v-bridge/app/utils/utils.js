/**
 * Creates the topic in which the MQTT client must publish to actuate in a device.
 *
 * @param tenant
 * @param deviceid
 */
function generateDojotActuationTopic(tenant, deviceid, suffix) {
  return `${tenant}:${deviceid}${suffix}`;
}

module.exports = { generateDojotActuationTopic };
