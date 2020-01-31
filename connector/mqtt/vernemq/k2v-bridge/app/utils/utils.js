/**
 * Creates the topic in which the MQTT client must publish to actuate in a device.
 *
 * @param tenant
 * @param deviceid
 */
function generateActuationTopic(tenant, deviceid) {
  return `${tenant}:${deviceid}/config`;
}

module.exports = { generateActuationTopic };
