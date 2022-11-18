import { Kafka, Logger } from '@dojot/microservice-sdk'

import { AppConfig, KafkaPayload } from 'src/types'

export class KafkaConsumer extends Kafka.Consumer {
  private callbackId: number | null

  constructor(private logger: Logger, private appconfig: AppConfig) {
    super({
      ...appconfig.sdk,
      'enable.async.commit': true,
      'kafka.consumer': appconfig.consumer,
      'kafka.topic': appconfig.topic,
    })

    this.logger.debug('constructor: Instantiating a kafka consumer', {})
    this.callbackId = null
  }

  init() {
    super.init()
  }

  initNewTenantEvent(callback: (payload: KafkaPayload) => void) {
    const topic = new RegExp(this.appconfig.subscribe['topics.regex.tenants'])
    this.callbackId = this.registerCallback(
      topic as unknown as string,
      callback,
    )
  }

  async isConnected() {
    try {
      const { connected } = await this.getStatus()
      return connected
    } catch (e: unknown) {
      this.logger.error('isConnected:', e as never)
      return false
    }
  }

  unregisterCallbacks() {
    if (this.callbackId) {
      this.unregisterCallback(this.callbackId)
      this.callbackId = null
      this.logger.debug(
        'unregisterCallbacks: Unregistered callback for tenant',
        {},
      )
    } else {
      this.logger.warn(
        'unregisterCallbacks: There is no callback to unregister',
        {},
      )
    }
  }
}