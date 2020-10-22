module.exports = {
  nodeEnv: process.env.NODE_ENV || 'production',
  app: {
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
  }
};
