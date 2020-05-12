module.exports = {
  app: {
    log_level: process.env.LOG_LEVEL || 'info',
  },
  kafka: {
    host: process.env.KAFKA_HOSTS || 'kafka:9092',
  },
  server: {
    host: process.env.KAFKA_WS_HOST || 'localhost',
    port: parseInt(process.env.KAFKA_WS_PORT, 10) || 8080,
  },
};
