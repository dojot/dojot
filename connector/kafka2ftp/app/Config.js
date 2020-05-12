const parseFTPS = (mode) => ((mode || false) && (mode.toString().toLowerCase().trim() === 'true' || Number(mode) > 0));

const config = {
  app: {
    logLevel: process.env.LOG_LEVEL || 'info',
  },
  kafka: {
    consumer: {
      'group.id': process.env.KAFKA_GROUP_ID || 'kafka2ftp',
      'metadata.broker.list': process.env.KAFKA_HOSTS || 'kafka:9092',
    },
  },
  endpoints: [{
    tenant: process.env.FTP_TENANT || 'admin',
    ftp: {
      host: process.env.FTP_HOST || 'localhost',
      port: process.env.FTP_PORT || 21,
      secure: parseFTPS(process.env.FTP_FTPS || false),
      secureOptions: null,
      user: process.env.FTP_USER || 'anonymous',
      password: process.env.FTP_PASSWORD || 'guest',
      maxConcurrentConnections: process.env.MAX_CONCURRENT_CONNECTIONS || 10,
      remoteDir: process.env.FTP_REMOTE_DIR || '/',
    },
  }],
  retryOptions: {
    retries: process.env.RETRIES || 13,
  },
};

module.exports = config;
