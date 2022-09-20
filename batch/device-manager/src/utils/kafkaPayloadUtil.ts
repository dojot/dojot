/**
 * Parses a kafka payload
 *
 * @param {KafkaPayload} payload a Kafka payload
 * @returns an object containing the kafka message
 */
export function getValue(payload: any): any {
  const { value } = payload;
  return JSON.parse(value.toString());
}