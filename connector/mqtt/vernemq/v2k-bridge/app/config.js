const { toBoolean } = require('./utils');

const app = {
  basedir: process.env.V2K_APP_BASEDIR,
  'connection.retry.count': parseInt(process.env.V2K_APP_CONNECTION_RETRY_COUNT, 0),
  'connection.retry.timeout': parseInt(process.env.V2K_APP_CONNECTION_RETRY_TIMEOUT, 0),
  'ejbca.address': process.env.V2K_APP_EJBCA_ADDRESS,
  hostname: process.env.V2K_APP_HOSTNAME,
};

const logger = {
  'transports.console.level': process.env.V2K_LOGGER_TRANSPORTS_CONSOLE_LEVEL,
  verbose: toBoolean(process.env.V2K_LOGGER_VERBOSE),
};

const messenger = {
  'produce.topic.suffix': process.env.V2K_MESSENGER_PRODUCE_TOPIC_SUFFIX,
};

const mqtt = {
  // Backpressure
  'backpressure.handlers': parseInt(process.env.V2K_MQTT_BACKPRESSURE_HANDLERS, 0),
  'backpressure.queue.length.max': parseInt(process.env.V2K_MQTT_BACKPRESSURE_QUEUE_LENGTH_MAX, 0),

  // Client
  'client.id': process.env.V2K_MQTT_CLIENT_ID,
  'client.keepalive': parseInt(process.env.V2K_MQTT_CLIENT_KEEPALIVE, 0),
  'client.secure': toBoolean(process.env.V2K_MQTT_CLIENT_SECURE),
  'client.subscription.qos': parseInt(process.env.V2K_MQTT_CLIENT_SUBSCRIPTION_QOS, 0),
  // eslint-disable-next-line no-useless-escape
  'client.subscription.topic': process.env.V2K_MQTT_CLIENT_SUBSCRIPTION_TOPIC,
  'client.username': process.env.V2K_MQTT_CLIENT_USERNAME,

  // Server
  'server.address': process.env.V2K_MQTT_SERVER_ADDRESS,
  'server.port': parseInt(process.env.V2K_MQTT_SERVER_PORT, 0),

  // TLS
  'tls.ca.file': process.env.V2K_MQTT_TLS_CA_FILE,
  'tls.certificate.file': process.env.V2K_MQTT_TLS_CERTIFICATE_FILE,
  'tls.key.file': process.env.V2K_MQTT_TLS_KEY_FILE,
};

const sdk = {
  'producer.connect.timeout.ms': parseFloat(process.env.V2K_SDK_PRODUCER_CONNECT_TIMEOUT_MS),
  'producer.disconnect.timeout.ms': parseFloat(process.env.V2K_SDK_PRODUCER_DISCONNECT_TIMEOUT_MS),
  'producer.flush.timeout.ms': parseFloat(process.env.V2K_SDK_PRODUCER_FLUSH_TIMEOUT_MS),
  'producer.pool.interval.ms': parseFloat(process.env.V2K_SDK_PRODUCER_POOL_INTERVAL_MS),
};

const kafka = {
  acks: parseInt(process.env.V2K_KAFKA_ACKS, 0),
  'batch.num.messages': parseInt(process.env.V2K_KAFKA_BATCH_NUM_MESSAGES, 0),
  'client.id': process.env.V2K_KAFKA_CLIENT_ID,
  'compression.codec': process.env.V2K_KAFKA_COMPRESSION_CODEC,
  dr_cb: (toBoolean(process.env.V2K_KAFKA_DR_CB)),
  'enable.idempotence': (toBoolean(process.env.V2K_KAFKA_ENABLE_IDEMPOTENCE)),
  'max.in.flight.requests.per.connection':
    parseInt(process.env.V2K_KAFKA_MAX_IN_FLIGHT_REQUESTS_PER_CONNECTION, 0),
  'metadata.broker.list': process.env.V2K_KAFKA_METADATA_BROKER_LIST,
  retries: parseInt(process.env.V2K_KAFKA_RETRIES, 0),
  'queue.buffering.max.kbytes': parseInt(process.env.V2K_KAFKA_QUEUE_BUFFERING_MAX_KBYTES, 0),
  'queue.buffering.max.ms': parseFloat(process.env.V2K_KAFKA_QUEUE_BUFFERING_MAX_MS),
  'retry.backoff.ms': parseInt(process.env.V2K_KAFKA_RETRY_BACKOFF_MS, 0),
  'socket.keepalive.enable':
    (toBoolean(process.env.V2K_KAFKA_SOCKET_KEEPALIVE_ENABLE)),
};

module.exports = {
  app, kafka, logger, messenger, mqtt, sdk,
};
