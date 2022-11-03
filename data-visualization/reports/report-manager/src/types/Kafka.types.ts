export type KafkaPayload = {
  value: string
}

export type KafkaParsedPayloadValue = {
  type: string
  tenant: string
  signatureKey: object
}
