const morgan = require('morgan');

/* HTTP request logger:
 * http://expressjs.com/en/resources/middleware/morgan.html */
module.exports = ({ logFormat, logger }) => {
  /* setup the logger */
  const defaultFormat = [
    ':remote-addr', '-', ':remote-user', ':id',
    '":method :url HTTP/:http-version"', ':status',
    ':res[content-length]', '":referrer"', '":user-agent"',
  ].join(' ');

  morgan.token('id', (req) => req.id);

  return {
    name: 'request-log-controller',
    middleware: morgan(logFormat || defaultFormat, {
      stream: {
        write: (message) => {
          logger.info(message);
        },
      },
    }),
  };
};
