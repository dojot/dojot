const { toBoolean } = require('./utils');

const logger = {
  'transports.console.level': process.env.V2K_LOGGER_TRANSPORTS_CONSOLE_LEVEL,
  verbose: toBoolean(process.env.V2K_LOGGER_VERBOSE),
};

const messenger = {
  'produce.topic.suffix': process.env.V2K_MESSENGER_PRODUCE_TOPIC_SUFFIX,
};

const mqtt = {
  // Backpressure
  'backpressure.handlers': parseInt(process.env.V2K_MQTT_BACKPRESSURE_HANDLERS, 10),
  'backpressure.queue.length.max': parseInt(process.env.V2K_MQTT_BACKPRESSURE_QUEUE_LENGTH_MAX, 10),

  // Client
  'client.id': process.env.V2K_MQTT_CLIENT_ID,
  'client.keepalive': parseInt(process.env.V2K_MQTT_CLIENT_KEEPALIVE, 10),
  'client.secure': toBoolean(process.env.V2K_MQTT_CLIENT_SECURE),
  'client.subscription.qos': parseInt(process.env.V2K_MQTT_CLIENT_SUBSCRIPTION_QOS, 10),
  'client.subscription.topic': process.env.V2K_MQTT_CLIENT_SUBSCRIPTION_TOPIC,
  'client.username': process.env.V2K_MQTT_CLIENT_USERNAME,

  // Server
  'server.address': process.env.V2K_MQTT_SERVER_ADDRESS,
  'server.port': parseInt(process.env.V2K_MQTT_SERVER_PORT, 10),

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
  'kafka.producer': {
    acks: parseInt(process.env.V2K_KAFKA_ACKS, 10),
    'client.id': process.env.V2K_KAFKA_CLIENT_ID,
    'compression.codec': process.env.V2K_KAFKA_COMPRESSION_CODEC,
    dr_cb: (toBoolean(process.env.V2K_KAFKA_DR_CB)),
    'enable.idempotence': (toBoolean(process.env.V2K_KAFKA_ENABLE_IDEMPOTENCE)),
    'max.in.flight.requests.per.connection':
    parseInt(process.env.V2K_KAFKA_MAX_IN_FLIGHT_REQUESTS_PER_CONNECTION, 10),
    'metadata.broker.list': process.env.V2K_KAFKA_METADATA_BROKER_LIST,
    retries: parseInt(process.env.V2K_KAFKA_RETRIES, 10),
    'queue.buffering.max.kbytes': parseInt(process.env.V2K_KAFKA_QUEUE_BUFFERING_MAX_KBYTES, 10),
    'queue.buffering.max.ms': parseFloat(process.env.V2K_KAFKA_QUEUE_BUFFERING_MAX_MS),
    'retry.backoff.ms': parseInt(process.env.V2K_KAFKA_RETRY_BACKOFF_MS, 10),
    'batch.num.messages': parseInt(process.env.V2K_KAFKA_BATCH_NUM_MESSAGES, 10),
    'socket.keepalive.enable': (toBoolean(process.env.V2K_KAFKA_SOCKET_KEEPALIVE_ENABLE)),
  },
  'kafka.topic': {
    'auto.offset.reset': 'latest',
  },
};

module.exports = {
  kafka, logger, messenger, mqtt, sdk,
};
