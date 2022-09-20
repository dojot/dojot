import { Kafka, Logger } from '@dojot/microservice-sdk';

/**
 * This class handles messages from dojot topics on kafka
 * @class
 */
export default class KafkaConsumer extends Kafka.Consumer {
  private idCallbackTenant: number;
  /**
   * Create an instance
   */
  constructor(private logger: Logger, private config: any) {
    super({
      ...config.sdk,
      'enable.async.commit': true,
      'kafka.consumer': config.consumer,
      'kafka.topic': config.topic,
    });
    this.logger.debug('constructor: Instantiating a Kafka Consumer', {});
    this.idCallbackTenant = null;
  }

  public init(): void {
    super.init();
  }

  public initNewTenantEvent(callback: Function) {
    const topic = new RegExp(this.config.subscribe['topics.regex.tenants']);
    // @ts-expect-error
    this.idCallbackTenant = this.registerCallback(topic, callback);
  }

  /**
   * A function to get if kafka is connected
   *
   * @returns {Promise<boolean>} if kafka is connect
   */
  async isConnected() {
    try {
      const { connected } = await this.getStatus();
      if (connected) {
        return true;
      }
      return false;
    } catch (e) {
      this.logger.error('isConnected:', e);
      return false;
    }
  }

  /**
   * Unregister all callbacks
   *
   * @throws If Cannot unregister callback
   */
  unregisterCallbacks() {
    if (this.idCallbackTenant) {
      this.unregisterCallback(this.idCallbackTenant);
      this.idCallbackTenant = null;
      this.logger.debug('unregisterCallbacks: Unregistered callback for tenant', {});
    } else {
      this.logger.warn('unregisterCallbacks: Doesn\'t exist Callback to unregister for tenant', {});
    }
  }
}