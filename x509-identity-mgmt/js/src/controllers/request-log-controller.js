const morgan = require('morgan');

/* HTTP request logger:
 * http://expressjs.com/en/resources/middleware/morgan.html */
module.exports = ({ logFormat, logger }) => {
  /* setup the logger */
  const defaultFormat = [
    ':id',
    '"HTTP/:http-version :method :url"',
    '":status :res[content-length]bytes :total-time[0]ms"',
    '":remote-addr :referrer :user-agent"',
  ].join(' ');

  morgan.token('id', (req) => req.id);

  return {
    name: 'request-log-controller',
    middleware: morgan(logFormat || defaultFormat, {
      stream: {
        write: (message) => {
          logger.info(message.slice(0, -1));
        },
      },
    }),
  };
};
