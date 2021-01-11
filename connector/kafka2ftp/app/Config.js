const parseBool = (mode) => ((mode || false) && (mode.toString().toLowerCase().trim() === 'true' || Number(mode) > 0));

const config = {
  log: {
    'console.level': process.env.LOG_CONSOLE_LEVEL || 'info',
    verbose: parseBool(process.env.LOG_VERBOSE || false),
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
      port: parseInt(process.env.FTP_PORT, 10) || 21,
      secure: parseBool(process.env.FTP_FTPS || false),
      secureOptions: null,
      user: process.env.FTP_USER || 'anonymous',
      password: process.env.FTP_PASSWORD || 'guest',
      maxConcurrentConnections: parseInt(process.env.MAX_CONCURRENT_CONNECTIONS, 10) || 10,
      remoteDir: process.env.FTP_REMOTE_DIR || '/',
    },
  }],
  retryOptions: {
    retries: parseInt(process.env.RETRIES, 10) || 13,
  },
};

module.exports = config;
