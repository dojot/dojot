import { KafkaParsedPayloadValue, KafkaPayload } from 'src/types'

export function getValue(payload: KafkaPayload): KafkaParsedPayloadValue {
  return JSON.parse(payload.value.toString())
}