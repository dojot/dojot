const { toBoolean } = require('./utils');

const app = {
  basedir: process.env.K2V_APP_BASEDIR,
  'connection.retry.count': parseInt(process.env.K2V_APP_CONNECTION_RETRY_COUNT, 10),
  'connection.retry.timeout': parseInt(process.env.K2V_APP_CONNECTION_RETRY_TIMEOUT, 10),
  'ejbca.address': process.env.K2V_APP_EJBCA_ADDRESS,
  hostname: process.env.K2V_APP_HOSTNAME,
};

const logger = {
  'transports.console.level': process.env.K2V_LOGGER_TRANSPORTS_CONSOLE_LEVEL,
  verbose: (toBoolean(process.env.K2V_LOGGER_VERBOSE)),
};

const messenger = {
  'consume.topic.suffix': process.env.K2V_MESSENGER_CONSUME_TOPIC_SUFFIX,
};

const mqtt = {
  // Client
  'client.id': process.env.K2V_MQTT_CLIENT_ID,
  'client.keepalive': parseInt(process.env.K2V_MQTT_CLIENT_KEEPALIVE, 10),
  'client.publish.qos': parseInt(process.env.K2V_MQTT_CLIENT_PUBLISH_QOS, 10),
  'client.publish.topic.suffix': process.env.K2V_MQTT_CLIENT_PUBLISH_TOPIC_SUFFIX,
  'client.secure': toBoolean(process.env.K2V_MQTT_CLIENT_SECURE),
  'client.username': process.env.K2V_MQTT_CLIENT_USERNAME,

  // Server
  'server.address': process.env.K2V_MQTT_SERVER_ADDRESS,
  'server.port': parseInt(process.env.K2V_MQTT_SERVER_PORT, 10),

  // TLS
  'tls.ca.file': process.env.K2V_MQTT_TLS_CA_FILE,
  'tls.certificate.file': process.env.K2V_MQTT_TLS_CERTIFICATE_FILE,
  'tls.key.file': process.env.K2V_MQTT_TLS_KEY_FILE,
};

const kafka = {
  'client.id': process.env.K2V_KAFKA_CLIENT_ID,
  'group.id': process.env.K2V_KAFKA_GROUP_ID,
  'max.in.flight.requests.per.connection':
    parseInt(process.env.K2V_KAFKA_MAX_IN_FLIGHT_REQUESTS_PER_CONNECTION, 10),
  'metadata.broker.list': process.env.K2V_KAFKA_METADATA_BROKER_LIST,
  'socket.keepalive.enable': (toBoolean(process.env.K2V_KAFKA_SOCKET_KEEPALIVE_ENABLE)),
};

const sdk = {
  'commit.interval.ms': parseInt(process.env.K2V_SDK_COMMIT_INTERVAL_MS, 10),
  'in.processing.max.messages': parseInt(process.env.K2V_SDK_IN_PROCESSING_MAX_MESSAGES, 10),
  'queued.max.messages.bytes': parseInt(process.env.K2V_SDK_QUEUED_MAX_MESSAGES_BYTES, 10),
  'subscription.backoff.delta.ms': parseInt(process.env.K2V_SDK_SUBSCRIPTION_BACKOFF_DELTA_MS, 10),
  'subscription.backoff.max.ms': parseInt(process.env.K2V_SDK_SUBSCRIPTION_BACKOFF_MAX_MS, 10),
  'subscription.backoff.min.ms': parseInt(process.env.K2V_SDK_SUBSCRIPTION_BACKOFF_MIN_MS, 10),
};

module.exports = {
  app,
  kafka,
  logger,
  messenger,
  mqtt,
  sdk,
};
