const crypto = require('crypto');
const { parseUint, parseBoolean, parseArray } = require('./Utils');

module.exports = {
  nodeEnv: process.env.NODE_ENV || 'production',
  app: {
    log: {
      log_console_level: process.env.LOG_LEVEL || 'info',
      log_verbose: parseBoolean(process.env.LOG_VERBOSE || false),
      log_file: parseBoolean(process.env.LOG_FILE || false),
      log_file_level: process.env.LOG_FILE_LEVEL || 'debug',
      log_file_filename: 'kafka-ws-logs-%DATE%.log',
    },
    /* Log settings */
    morganLogFormat: ([
      ':remote-addr',
      '-',
      ':remote-user',
      ':id',
      '":method :url HTTP/:http-version"',
      ':status',
      ':res[content-length]',
      '":referrer"',
      '":user-agent"',
    ]).join(' '),
    ticket: {
      secret: process.env.TICKET_SECRET || crypto.randomFillSync(Buffer.alloc(10)).toString('hex'),
      expiresIn: parseUint(process.env.TICKET_EXPIRATION_SEC) || 60,
    },
  },
  redis: {
    host: process.env.REDIS_HOST || 'kafka-ws-redis',
    port: parseInt(process.env.REDIS_PORT, 10) || 6379,
    database: parseInt(process.env.REDIS_DATABASE, 10) || 1,
  },
  kafka: {
    'kafka.consumer': {
      'group.id': process.env.KAFKA_GROUP_ID || 'kafka-ws',
      'metadata.broker.list': process.env.KAFKA_HOSTS || 'kafka:9092',
    },
    'kafka.topic': {
      'auto.offset.reset': 'largest',
    },
  },
  server: {
    host: process.env.KAFKA_WS_HOST || '0.0.0.0',
    port: parseInt(process.env.KAFKA_WS_PORT, 10) || 8080,
    tls: parseBoolean(process.env.KAFKA_WS_TLS || false),
    tls_ca_file: process.env.KAFKA_WS_TLS_CA_FILE || '/opt/kafka-ws/certs/ca-cert.pem',
    tls_key_file: process.env.KAFKA_WS_TLS_KEY_FILE || '/opt/kafka-ws/certs/server-key.pem',
    tls_cert_file: process.env.KAFKA_WS_TLS_CERT_FILE || '/opt/kafka-ws/certs/server-cert.pem',
    jwt_exp_time: parseBoolean(process.env.KAFKA_WS_JWT_EXP_TIME || false),
    connection_max_life_time: parseInt(process.env.KAFKA_WS_MAX_LIFE_TIME, 10) || 7200,
    request_cert: parseBoolean(process.env.KAFKA_WS_REQUEST_CERT || false),
  },
  /* https://github.com/godaddy/terminus settings */
  terminus: {
    /* use object returned from /healthcheck in response */
    verbatim: parseBoolean(process.env.TERMINUS_VERBATIM || true),
    /* number of milliseconds before forceful exiting */
    timeout: parseUint(process.env.TERMINUS_TIMEOUT_MS) || 5000,
    /* array of signals to listen for relative to shutdown */
    signals: parseArray(process.env.TERMINUS_SIGNALS) || ['SIGINT', 'SIGTERM'],
  },
};
