const parseBoolean = (mode) => ((mode || false) && (mode.toString().toLowerCase().trim() === 'true' || Number(mode) > 0));

module.exports = {
  app: {
    log_level: process.env.LOG_LEVEL || 'info',
    log_verbose: parseBoolean(process.env.LOG_VERBOSE || false),
    log_file: parseBoolean(process.env.LOG_FILE || false),
    log_file_level: process.env.LOG_FILE_LEVEL || 'debug',
    log_file_filename: 'kafka-ws-logs-%DATE%.log',
  },
  kafka: {
    consumer: {
      kafka: {
        'group.id': process.env.KAFKA_GROUP_ID || 'kafka-ws',
        'metadata.broker.list': process.env.KAFKA_HOSTS || 'kafka:9092',
      },
    },
  },
  server: {
    host: process.env.KAFKA_WS_HOST || '0.0.0.0',
    port: parseInt(process.env.KAFKA_WS_PORT, 10) || 8080,
    tls: parseBoolean(process.env.KAFKA_WS_TLS || false),
    ca_file: process.env.KAFKA_WS_TLS_CA_FILE || '/opt/kafka-ws/certs/ca-cert.pem',
    key_file: process.env.KAFKA_WS_TLS_KEY_FILE || '/opt/kafka-ws/certs/server-key.pem',
    cert_file: process.env.KAFKA_WS_TLS_CERT_FILE || '/opt/kafka-ws/certs/server-cert.pem',
  },
};
