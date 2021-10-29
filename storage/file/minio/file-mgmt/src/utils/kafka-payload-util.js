/**
 * Parses a kafka payload
 *
 * @param {KafkaPayload} payload a Kafka payload
 * @returns an object containing the kafka message
 */
function getValue(payload) {
  const { value } = payload;
  return JSON.parse(value.toString());
}

module.exports = {
  getValue,
};
