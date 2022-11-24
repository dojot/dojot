import { Kafka, Logger } from '@dojot/microservice-sdk'

import { Config } from 'src/types'

import { TenantManager } from './TenantManager.kafka'

type Payload = {
  value: string
}

type ParsedPayloadValue = {
  type: 'CREATE' | 'DELETE'
  tenant: string
  signatureKey: object
}

export class KafkaConsumer extends Kafka.Consumer {
  private callbackId: number | null = null

  constructor(
    private logger: Logger,
    private appConfig: Config,
    private tenantManager: TenantManager,
  ) {
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
    this.registerTenantEvent()
  }

  getParsedValue(payload: Payload): ParsedPayloadValue {
    return JSON.parse(payload.value.toString())
  }

  async handleTenantEvent(payload: Payload, callback: () => void) {
    try {
      this.logger.debug('handleTenantEvent: handling tenant event', {})

      const operations = {
        CREATE: this.tenantManager.create.bind(this.tenantManager),
        DELETE: this.tenantManager.delete.bind(this.tenantManager),
      }

      this.logger.debug(`handleTenantEvent: parsing payload`, {})
      const value = this.getParsedValue(payload)

      this.logger.debug('handleTenantEvent: calling operation', {
        type: value.type,
        tenant: value.tenant,
      })

      const operation = operations[value.type]

      if (operation) {
        await operation(value)
        this.logger.debug('handleTenantEvent: operation ran successfully', {})
      } else {
        this.logger.debug('handleTenantEvent: operation not implemented', {})
      }
    } catch (e) {
      this.logger.error(
        'handleTenantEvent: failed to handle tenant event',
        e as never,
      )
    } finally {
      this.logger.debug('handleTenantEvent: calling kafka callback', {})
      if (callback) callback()
    }
  }

  registerTenantEvent() {
    const topic = new RegExp(this.appConfig.subscribe['topics.regex.tenants'])
    this.callbackId = this.registerCallback(
      topic as unknown as string,
      this.handleTenantEvent.bind(this),
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
