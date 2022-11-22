import { Kafka, Logger } from '@dojot/microservice-sdk'

import { Config, KafkaPayload } from 'src/types'

export class KafkaConsumer extends Kafka.Consumer {
  private callbackId: number | null = null

  constructor(private logger: Logger, private appConfig: Config) {
    super({
      ...appConfig.sdk,
      'enable.async.commit': true,
      'kafka.consumer': appConfig.consumer,
      'kafka.topic': appConfig.topic,
    })

    this.logger.debug('constructor: kafka consumer instantiated', {})
  }

  async init() {
    await super.init()
  }

  registerTenantEvent(callback: (payload: KafkaPayload) => void) {
    const topic = new RegExp(this.appConfig.subscribe['topics.regex.tenants'])
    this.callbackId = this.registerCallback(
      topic as unknown as string,
      callback,
    )
  }

  async isConnected() {
    try {
      const { connected } = await this.getStatus()
      return connected
    } catch (e) {
      this.logger.error('isConnected', e as never)
      return false
    }
  }

  unregisterCallbacks() {
    if (this.callbackId) {
      this.unregisterCallback(this.callbackId)
      this.callbackId = null
      this.logger.debug(
        'unregisterCallbacks: unregistered callback for tenant',
        {},
      )
    } else {
      this.logger.warn(
        'unregisterCallbacks: there is no callback to unregister',
        {},
      )
    }
  }
}
