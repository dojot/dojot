const parseBoolean = (mode) => ((mode || false) && (mode.toString().toLowerCase().trim() === 'true' || Number(mode) > 0));

module.exports = {
  app: {
    log: {
      log_console_level: process.env.LOG_LEVEL || 'info',
      log_verbose: parseBoolean(process.env.LOG_VERBOSE || false),
      log_file: parseBoolean(process.env.LOG_FILE || false),
      log_file_level: process.env.LOG_FILE_LEVEL || 'debug',
      log_file_filename: 'kafka-ws-logs-%DATE%.log',
    },
  },
  redis: {
    host: process.env.REDIS_HOST || 'redis',
    port: parseInt(process.env.REDIS_PORT, 10) || 6379,
    database: parseInt(process.env.REDIS_DATABASE, 10) || 1,
  },
  kafka: {
    consumer: {
      kafka: {
        'group.id': process.env.KAFKA_GROUP_ID || 'kafka-ws',
        'metadata.broker.list': process.env.KAFKA_HOSTS || 'kafka:9092',
        'auto.offset.reset': 'largest',
      },
    },
  },
  server: {
    host: process.env.KAFKA_WS_HOST || '0.0.0.0',
    port: parseInt(process.env.KAFKA_WS_PORT, 10) || 8080,
    tls: parseBoolean(process.env.KAFKA_WS_TLS || false),
    tls_ca_file: process.env.KAFKA_WS_TLS_CA_FILE || '/opt/kafka-ws/certs/ca-cert.pem',
    tls_key_file: process.env.KAFKA_WS_TLS_KEY_FILE || '/opt/kafka-ws/certs/server-key.pem',
    tls_cert_file: process.env.KAFKA_WS_TLS_CERT_FILE || '/opt/kafka-ws/certs/server-cert.pem',
    jwt_header_auth: parseBoolean(process.env.KAFKA_WS_JWT_HEADER_AUTH || false),
    jwt_exp_time: parseBoolean(process.env.KAFKA_WS_JWT_EXP_TIME || false),
    connection_max_life_time: parseInt(process.env.KAFKA_WS_MAX_LIFE_TIME, 10) || 7200,
  },
};
