const { toBoolean } = require('./utils');

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
  'kafka.consumer': {
    'client.id': process.env.K2V_KAFKA_CLIENT_ID,
    'group.id': process.env.K2V_KAFKA_GROUP_ID,
    'metadata.broker.list': process.env.K2V_KAFKA_METADATA_BROKER_LIST,
    'socket.keepalive.enable': (toBoolean(process.env.K2V_KAFKA_SOCKET_KEEPALIVE_ENABLE)),
    'max.in.flight.requests.per.connection':
    parseInt(process.env.K2V_KAFKA_MAX_IN_FLIGHT_REQUESTS_PER_CONNECTION, 10),
  },
  'kafka.topic': {
    'topic.auto.offset.reset': 'beginning'
  }
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
  kafka,
  logger,
  messenger,
  mqtt,
  sdk,
};
