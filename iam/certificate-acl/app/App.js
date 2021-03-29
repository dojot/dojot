const util = require('util');
const {
  ConfigManager: { getConfig },
  Logger,
} = require('@dojot/microservice-sdk');

const KafkaConsumer = require('./kafka/KafkaConsumer');

const RedisManager = require('./redis/RedisManager');
const StateManager = require('./StateManager');

// redis instance
const redisClient = RedisManager.getClient();

const logger = new Logger('certificate-acl:app');

// configuration
const CERTIFICATE_ACL_CONFIG_LABEL = 'CERTIFICATE_ACL';
const config = getConfig(CERTIFICATE_ACL_CONFIG_LABEL);

const CERTIFICATE_ACL_CREATE_EVENT_TYPE = 'create';
const CERTIFICATE_ACL_DELETE_EVENT_TYPE = 'delete';
const CERTIFICATE_ACL_UPDATE_EVENT_TYPE = 'update.delta';

const incommingMessagesCallback = (data) => {
  const { value: payload } = data;

  try {
    const jsonReceived = JSON.parse(payload.toString());
    logger.debug(`Message received! ${util.inspect(jsonReceived, false, 5, true)}`);

    const keyFingerprint = jsonReceived.data.eventData.fingerprint;
    const { tenant } = jsonReceived.metadata;
    let valueUsername = jsonReceived.data.eventData.belongsTo.device;

    const { eventType } = jsonReceived.data;

    if (tenant) {
      valueUsername = `${tenant}:${valueUsername}`;
    }

    if (!keyFingerprint) {
      logger.info(`Invalid fingerprint received ${keyFingerprint}, ignoring.....`);
      return;
    }

    switch (eventType) {
      case CERTIFICATE_ACL_CREATE_EVENT_TYPE:
      case CERTIFICATE_ACL_UPDATE_EVENT_TYPE:
        logger.info(`Saving to redis the key pair ${keyFingerprint} - ${valueUsername}`);
        redisClient.set(keyFingerprint, valueUsername);
        break;

      case CERTIFICATE_ACL_DELETE_EVENT_TYPE:
        logger.info(`Deleting in redis the key ${keyFingerprint}`);
        redisClient.del(keyFingerprint);
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

    const topics = new RegExp(`^.+\\.${topicSuffix}`);
    kafkaConsumer.registerCallback(topics, incommingMessagesCallback);

    // healthchecker and shutdown
    const healthCheckerInterval = config.healthcheck['kafka.interval.ms'];
    StateManager.addHealthChecker('kafka', kafkaConsumer.checkHealth.bind(kafkaConsumer), healthCheckerInterval);
    StateManager.registerShutdownHandler(kafkaConsumer.shutdownProcess.bind(kafkaConsumer));
  }).catch((err) => {
    logger.error(`Error initializing the kafka consumer exiting!... ${err}`);
    process.exit(1);
  });
};

module.exports = { init };
