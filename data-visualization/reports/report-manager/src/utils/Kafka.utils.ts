import { KafkaParsedPayloadValue, KafkaPayload } from 'src/types'

export class KafkaUtils {
  getValue(payload: KafkaPayload): KafkaParsedPayloadValue {
    return JSON.parse(payload.value.toString())
  }
}
