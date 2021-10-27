/**
 * Parses a kafka payload
 *
 * @param {KafkaPayload} payload a Kafka payload
 * @returns an object containing the kafka message
 */
function getValue(payload) {
  const { value } = payload;
  const payloadObject = JSON.parse(value.toString());
  return payloadObject;
}

module.exports = {
  getValue,
};
