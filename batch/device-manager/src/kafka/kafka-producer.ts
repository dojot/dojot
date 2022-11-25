import { Kafka, Logger } from '@dojot/microservice-sdk';

import { AppConfig } from 'src/types';

export enum EventKafka {
  CREATE = 'create',
  UPDATE = 'update',
  REMOVE = 'remove',
}

class NotificationMessageProducer {
  event: string;
  data: string;
  meta: string;

  constructor(event: string, data: string, meta: string) {
    this.event = event;
    this.data = data;
    this.meta = meta;
  }

  to_json(): string {
    return JSON.stringify({
      event: this.event,
      data: this.data,
      meta: { service: this.meta },
    });
  }
}

/**
 * This class handles messages from dojot topics on kafka
 * @class
 */
export class KafkaProducer extends Kafka.Producer {
  /**
   * Create an instance
   */
  constructor(private logger: Logger, private appconfig: AppConfig) {
    super({
      ...appconfig.sdk,
      'kafka.producer': appconfig.producer,
      'kafka.topic': appconfig.topic,
    });
    this.logger.debug('constructor: Instantiating a Kafka Producer', {});
  }

  public init(): void {
    this.logger.info('Initializing Kafka Producer...', {});
    this.connect()
      .then(() => {
        this.logger.info('... Kafka Producer was initialized', {});
      })
      .catch((error) => {
        this.logger.error(
          'An error occurred while initializing the Agent Messenger. Bailing out!',
          {},
        );
        this.logger.error(error.stack || error, {});
        process.exit(1);
      });
  }

  async send(event: string, tenant: string, deviceId: string) {
    const topicSuffix = this.appconfig.message['produce.topic.subject'];

    const full_msg = new NotificationMessageProducer(
      event,
      deviceId,
      tenant,
    ).to_json();
    this.logger.debug(`Mount of message full  ${full_msg}...`, {});
    try {
      // publish
      const kafkaTopic = `${tenant}.${topicSuffix}`;
      const stringMessage = full_msg;
      const messageKey = `${tenant}:${deviceId}`;
      const partition = 0;

      this.logger.debug(
        `Trying to send message to kafka topic ${kafkaTopic}...`,
        {},
      );

      await this.produce(kafkaTopic, stringMessage, messageKey, partition);

      this.logger.debug(
        `Successfully sent message to Kafka in ${kafkaTopic}`,
        {},
      );
      this.logger.debug(
        `Published message ${stringMessage} to ${tenant}/${topicSuffix}`,
        {},
      );
      return;
    } catch (error) {
      this.logger.error(
        `Failed to publish message to ${tenant}/${topicSuffix} (${error}).`,
        {},
      );
      throw new Error(
        `Failed to publish message to ${tenant}/${topicSuffix} (${error}).`,
      );
    }
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
      this.logger.error('isConnected:', { e });
      return false;
    }
  }

  async CloseConnected() {
    try {
      const { connected } = await this.getStatus();
      if (connected) {
        this.disconnect();
      }
    } catch (e) {
      this.logger.error('isConnected:', { e });
    }
  }

  registerShutdown() {
    /*
    this.serviceState.registerShutdownHandler(async () => {
      logger.warn('Shutting down Kafka connection...');
      return this.producer.disconnect();
    });*/
  }
}
