const { promisify } = require('util');
const {
  ConfigManager: { getConfig },
  Logger,
} = require('@dojot/microservice-sdk');

const KafkaConsumer = require('./kafka/KafkaConsumer');

const RedisManager = require('./redis/RedisManager');
const StateManager = require('./StateManager');

// redis instance
const redisClient = RedisManager.getClient();
const redisAsyncSet = promisify(redisClient.set).bind(redisClient);
const redisAsyncDel = promisify(redisClient.del).bind(redisClient);

const logger = new Logger('certificate-acl:app');

// configuration
const CERTIFICATE_ACL_CONFIG_LABEL = 'CERTIFICATE_ACL';
const config = getConfig(CERTIFICATE_ACL_CONFIG_LABEL);

const CERTIFICATE_ACL_CREATE_EVENT_TYPE = 'ownership.create';
const CERTIFICATE_ACL_DELETE_EVENT_TYPE = 'ownership.delete';
const CERTIFICATE_ACL_UPDATE_EVENT_TYPE = 'ownership.update';

const incommingMessagesCallback = async (data, ack) => {
  const { value: payload } = data;

  try {
    const jsonReceived = JSON.parse(payload.toString());

    const keyFingerprint = jsonReceived.data.eventData.fingerprint;
    const { tenant } = jsonReceived.metadata;
    let ownerIdentifier = jsonReceived.data.eventData.belongsTo.device
      || jsonReceived.data.eventData.belongsTo.application;

    const { eventType } = jsonReceived.data;

    if (tenant) {
      ownerIdentifier = `${tenant}:${ownerIdentifier}`;
    }

    if (!keyFingerprint) {
      logger.info(`Invalid fingerprint received ${keyFingerprint}, ignoring.....`);
      return;
    }

    switch (eventType) {
      case CERTIFICATE_ACL_CREATE_EVENT_TYPE:
      case CERTIFICATE_ACL_UPDATE_EVENT_TYPE:
        logger.info(`Saving to redis the key pair ${keyFingerprint} - ${ownerIdentifier}`);
        redisAsyncSet(keyFingerprint, ownerIdentifier).then((value) => {
          logger.debug(`Data sucesffully saved to redis! ${value}`);
          ack();
        }).catch((err) => {
          logger.warn(`Error saving data in redis! ${err}`);
        });
        break;

      case CERTIFICATE_ACL_DELETE_EVENT_TYPE:
        logger.info(`Deleting in redis the key ${keyFingerprint}`);
        redisAsyncDel(keyFingerprint).then((value) => {
          logger.debug(`Data sucesffully saved to redis! ${value}`);
          ack();
        }).catch((err) => {
          logger.warn(`Error deliting data in redis! ${err}`);
        });
        break;

      default:
        logger.warn(`Invalid event type ${eventType} .... ingnoring payload.`);
        break;
    }
  } catch (error) {
    logger.warn(`invalid data received, descarding... ${error}`);
  }
};

const init = () => {
  logger.info('Initializing app');

  const kafkaConsumer = new KafkaConsumer();
  kafkaConsumer.init().then(() => {
    logger.info('Kafka Consumer initialized sucessfully!');

    const topicSuffix = config.app['consumer.topic.suffix'].replace(/\./g, '\\.');

    const topics = new RegExp(`^.*${topicSuffix}`);
    kafkaConsumer.registerCallback(topics, incommingMessagesCallback);

    // TODO
    // query x.509-identity-mgmt and guarantee that the Redis is synced with it.

  }).catch((err) => { 
    logger.error(`Error initializing the kafka consumer exiting!... ${err}`);
    StateManager.shutdown();
  });
};

module.exports = { init };
