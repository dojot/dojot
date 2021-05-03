const util = require('util');
const {
  ConfigManager: { getConfig },
  Logger,
} = require('@dojot/microservice-sdk');
const KafkaConsumer = require('./kafka/KafkaConsumer');
const RedisManager = require('./redis/RedisManager');
const StateManager = require('./StateManager');

const logger = new Logger('certificate-acl:app');

// configuration
const CERTIFICATE_ACL_CONFIG_LABEL = 'CERTIFICATE_ACL';
const config = getConfig(CERTIFICATE_ACL_CONFIG_LABEL);

const CERTIFICATE_ACL_CREATE_EVENT_TYPE = 'ownership.create';
const CERTIFICATE_ACL_DELETE_EVENT_TYPE = 'ownership.delete';
const CERTIFICATE_ACL_UPDATE_EVENT_TYPE = 'ownership.update';

class Application {
  /**
   * Instantiates the application
   */
  constructor() {
    // instantiate Kafka Consumer
    this.kafkaConsumer = new KafkaConsumer();

    // instantiate Redis Manager
    this.redisManager = new RedisManager();
    this.redisManager.on('healthy', () => this.kafkaConsumer.resume());
    this.redisManager.on('unhealthy', () => this.kafkaConsumer.suspend());
  }

  /**
   * Processing callback to be passed to the Kafka Consumer.
   *
   * @param {*} data
   * @param {*} ack
   *
   * @private
   */
  async processData(data, ack) {
    const terminate = (error) => {
      // if you get here, it's because the message cannot be processed anyway
      // so, it's better to terminate the service
      logger.error('Service will be closed: ', error);
      process.kill(process.pid, 'SIGTERM');
    };

    try {
      const jsonData = JSON.parse(data.value);
      logger.debug(`Processing event: ${util.inspect(jsonData, { depth: 3 })}`);

      const {
        metadata: { tenant } = {},
        data: {
          eventType,
          eventData: {
            fingerprint,
            belongsTo: { device, application } = {},
          } = {},
        } = {},
      } = jsonData;
      const owner = ((tenant) ? `${tenant}:${device}` : application);

      switch (eventType) {
        case CERTIFICATE_ACL_CREATE_EVENT_TYPE:
        case CERTIFICATE_ACL_UPDATE_EVENT_TYPE:
          if (!fingerprint || (!(tenant && device) && !application)) {
            terminate(
              `Missing mandatory attributes in processing event: ${eventType}`,
            );
            return;
          }
          // if returns here, will make sync because of an 'await' in the
          // kafka-consumer ...
          // Maybe would be interesting to have a nack callback too
          this.redisManager.setAsync(fingerprint, owner).then(() => {
            logger.debug(`Added to Redis: ${fingerprint} -> ${owner}`);
            ack();
          }).catch((error) => terminate(
            `Failed to add to Redis ${fingerprint} -> ${owner} (${error}).`,
          ));
          break;
        case CERTIFICATE_ACL_DELETE_EVENT_TYPE:
          if (!fingerprint) {
            terminate(
              `Missing mandatory attributes in processing event: ${eventType}`,
            );
            return;
          }
          // if returns here, will make sync because of an 'await' in the
          // kafka-consumer ..
          this.redisManager.delAsync(fingerprint).then(() => {
            logger.debug(`Removed from Redis: ${fingerprint}`);
            ack();
          }).catch((error) => terminate(
            `Failed to remove from Redis ${fingerprint} -> ${owner} (${error}).`,
          ));
          break;
        default:
          logger.warn(`Unexpected eventType: ${eventType} (discarded)`);
          ack();
      }
    } catch (error) {
      // if you get here, it's because the message cannot be processed anyway
      // so, it's better to terminate the service
      // another approach would be discard this message, calling ack()
      terminate(
        `Failed to process message ${util.inspect(data)}. `
        + `Error: ${error}`,
      );
    }
  }

  /**
   * Initializes the application.
   * Starts consuming from Kafka.
   *
   * @returns
   */
  init() {
    return this.kafkaConsumer.init().then(() => {
      // register processing callback
      this.kafkaConsumer.registerCallback(
        new RegExp(config.app['consumer.topic']),
        this.processData.bind(this),
      );
    });
  }
}

module.exports = Application;
