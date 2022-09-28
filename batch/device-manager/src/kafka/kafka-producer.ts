import { Kafka, Logger } from '@dojot/microservice-sdk';

enum EventDevices {
  CREATE = "create",
  UPDATE = "update",
  REMOVE = "remove",
}

class NotificationMessageProducer
{
  event: string;
  data:  String;
  meta:  String;

  constructor(event:string,data:string,meta:string)
  {
    this.event = event;
    this.data = data;
    this.meta = meta;
  }

  to_json():string
  {
    return JSON.stringify({"event": this.event, "data": this.data, "meta": this.meta})
  }
}


/**
 * This class handles messages from dojot topics on kafka
 * @class
 */
export default class KafkaProducer extends Kafka.Producer{
  private idCallbackTenant: number;
  /**
   * Create an instance
   */
  constructor(private logger: Logger, private config: any) {
    super({
      ...config.sdk,
      'kafka.consumer': config.producer,
      'kafka.topic': config.topic,
    });
    this.logger.debug('constructor: Instantiating a Kafka Producer', {});
    this.idCallbackTenant = 0;
  }

  public init(): void {
    super.connect();
  }

  async send(event: string, tenant: string, deviceId: string) {
    const topicSuffix = this.config.messenger.produce.topic.suffix;

    const full_msg = new NotificationMessageProducer(event,tenant,deviceId).to_json();
    this.logger.debug(`Mount of message full  ${full_msg}...`,{});
    try {
      // publish
      const kafkaTopic = `${tenant}.${topicSuffix}`;
      const stringMessage =full_msg;
      const messageKey = `${tenant}:${deviceId}`;
      const partition = 0;
      

      this.logger.debug(`Trying to send message to kafka topic ${kafkaTopic}...`,{});

      await this.produce(kafkaTopic, stringMessage, messageKey, partition);
           

      this.logger.debug(`Successfully sent message to Kafka in ${kafkaTopic}`,{});
      this.logger.debug(
        `Published message ${stringMessage} to ${tenant}/${topicSuffix}`,{});
      return;
    } catch (error) {
      this.logger.error(
        `Failed to publish message to ${tenant}/${topicSuffix} (${error}).`,{});
      throw new Error(`Failed to publish message to ${tenant}/${topicSuffix} (${error}).`);
    }
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
      this.logger.error('isConnected:', {e});
      return false;
    }
  }


}