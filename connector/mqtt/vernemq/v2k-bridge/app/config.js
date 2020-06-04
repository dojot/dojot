const { toBoolean, unsecuredMode } = require('./utils');

const app = {
  baseDir: process.env.BASE_DIR || '/opt/v2k_bridge',
  hostname: process.env.HOSTNAME || 'v2k-bridge',
  logger: {
    transports: {
      console: {
        level: process.env.V2K_CONSOLE_LOG_LEVEL || 'info',
      },
    },
    verbose: (toBoolean(process.env.V2K_LOG_VERBOSE)) || true,
  },
};

const messenger = {
  kafkaTopic: process.env.V2K_KAFKA_PRODUCE_TOPIC || 'device-data',
};

const mqtt = {
  clientUsername: process.env.V2K_MQTT_USERNAME || 'v2k-bridge',
  clientId: process.env.V2K_MQTT_CLIENT_ID || app.hostname,
  host: process.env.V2K_MQTT_HOST || 'vernemq-k8s',
  port: parseInt(process.env.V2K_MQTT_PORT, 0) || 8883,
  keepalive: parseInt(process.env.V2K_MQTT_KEEPALIVE, 0) || 60,
  secure: unsecuredMode(process.env.V2K_MQTT_SECURE || true),
  // eslint-disable-next-line no-useless-escape
  subscribeTopic: process.env.V2K_MQTT_SUBSCRIPTION_TOPIC || '\$share/group/+/attrs',
  subscribeQos: parseInt(process.env.V2K_MQTT_SUBSCRIPTION_QOS, 0) || 1,
  parallelHandlers: parseInt(process.env.V2K_BACKPRESSURE_PARALLEL_HANDLERS, 0) || 4,
  maxQueueLength: parseInt(process.env.V2K_BACKPRESSURE_MAX_QUEUE_LENGTH, 0) || 1048576,
  tls: {
    ca: {
      location: process.env.V2K_MQTT_CA_FILE || `${app.baseDir}/app/cert/ca.crt`,
    },
    certificate: {
      location: process.env.V2K_MQTT_CERT_FILE || `${app.baseDir}/app/cert/${app.hostname}.crt`,
    },
    privateKey: {
      location: process.env.V2K_MQTT_KEY_FILE || `${app.baseDir}/app/cert/${app.hostname}.key`,
    },
  },
};

const producer = {
  'producer.flush.timeout.ms': parseFloat(process.env.V2K_PRODUCER_FLUSH_TIMEOUT_MS) || 2000,
  'producer.pool.interval.ms': parseFloat(process.env.V2K_PRODUCER_POOL_INTERVAL_MS) || 100,
  'producer.connect.timeout.ms': parseFloat(process.env.V2K_PRODUCER_CONNECT_TIMEOUT_MS) || 5000,
  'producer.disconnect.timeout.ms': parseFloat(process.env.V2K_PRODUCER_DISCONNECT_TIMEOUT_MS)
    || 10000,
  kafka: {
    'client.id': process.env.V2K_KAFKA_CLIENT_ID || app.hostname,
    'metadata.broker.list': process.env.V2K_KAFKA_METADATA_BROKER_LIST || 'kafka-server:9092',
    'enable.idempotence': (toBoolean(process.env.V2K_KAFKA_ENABLE_IDEMPOTENCE))
      || false,
    acks: parseInt(process.env.V2K_KAFKA_ACKS, 0) || -1,
    retries: parseInt(process.env.V2K_KAFKA_RETRIES, 0) || 2,
    'max.in.flight.requests.per.connection':
      parseInt(process.env.V2K_KAFKA_MAX_IN_FLIGHT_REQUESTS_PER_CONNECTION, 0) || 1000000,
    'retry.backoff.ms': parseInt(process.env.V2K_KAFKA_RETRY_BACKOFF_MS, 0) || 100,
    'socket.keepalive.enable':
      (toBoolean(process.env.V2K_KAFKA_SOCKET_KEEPALIVE_ENABLE)) || false,
    'queue.buffering.max.kbytes': parseInt(process.env.V2K_KAFKA_QUEUE_BUFFERING_MAX_KBYTES, 0)
      || 1048576,
    'queue.buffering.max.ms': parseFloat(process.env.V2K_KAFKA_QUEUE_BUFFERING_MAX_MS) || 0.5,
    'batch.num.messages': parseInt(process.env.V2K_KAFKA_BATCH_NUM_MESSAGES, 0) || 10000,
    'compression.codec': process.env.V2K_KAFKA_COMPRESSION_CODEC || 'none',
    dr_cb: (toBoolean(process.env.V2K_KAFKA_DR_CB)) || true,
  },
};

module.exports = {
  app, messenger, mqtt, producer,
};
