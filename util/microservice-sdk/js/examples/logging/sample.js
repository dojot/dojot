const { Logger } = require('../index.js');

// By default, no transport is enabled; so you need to
// set at least one before to log the messages.
// It's worth to say that the transports are shared by all
// modules of the microservice; consequently, any change
// in the configuration will be valid for all!

// Setting console transport
// For more information about it, see:
// https://github.com/winstonjs/winston/blob/HEAD/docs/transports.md#console-transport
//
// This is the expected transport to be used in a Docker container
// once it is stateless.
Logger.setTransport('console', {
  // Any configuration put here will be merged with the defaults:
  // { level: 'info',
  //   format: consoleFormat /* customized format */}
  level: 'debug',
});

// Setting file transport
// For more information about it, see:
// https://github.com/winstonjs/winston-daily-rotate-file#readme
//
// This transport should be used if you need to keep the logs in
// files with rotation.
// This is not typically the case for Docker container applications,
// where the logs are expected to be redirected to /dev/stdout
Logger.setTransport('file', {
  // Any configuration put here will be merged with the defaults:
  // { level: 'info',
  //   dirname: '/var/log/',
  //   filename: 'dojot.microservice-%DATE%.log',
  //   datePattern: 'YYYY-MM-DD',
  //   zippedArchive: true,
  //   maxSize: '10m',
  //   maxFiles: '7d',
  //   format: fileFormat, /* customized format */
  //   level: 'debug',
  // }
  level: 'debug',
  filename: 'sample-app-%DATE%.log',
});

// Instantiate a logger with the service/module name
const logger = new Logger('sample-app');

setInterval(() => {
  // log simple message with different logging levels
  logger.debug('message #1');
  logger.info('message #2');
  logger.warn('message #3');
  logger.error('message #4');

  // log message with filename and line metadata (verbose mode)
  logger.debugv('message #5');
  logger.infov('message #6');
  logger.warnv('message #7');
  logger.errorv('message #8');

  // log message with additional metadata
  // you can also use the verbose version
  logger.debug('message #9', {
    rid: '7e921802-aa06-46c7-b4ba-1f6c2812d01d',
    src_ip: '192.168.127.99',
    tenant: 'admin',
  });
  logger.info('message #10', {
    rid: '7e921802-aa06-46c7-b4ba-1f6c2812d01d',
    src_ip: '192.168.127.99',
    tenant: 'admin',
  });
  logger.warn('message #11', {
    rid: '7e921802-aa06-46c7-b4ba-1f6c2812d01d',
    src_ip: '192.168.127.99',
    tenant: 'admin',
  });
  logger.error('message #12', {
    rid: '7e921802-aa06-46c7-b4ba-1f6c2812d01d',
    src_ip: '192.168.127.99',
    tenant: 'admin',
  });
}, 5000);
