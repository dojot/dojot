const { Logger } = require('@dojot/microservice-sdk');
const { logSampleMessages } = require('./SampleModule');

// By default, console transport is enabled; but you can
// change it or add a file transport if required.
// It's worth to say that the transports are shared by all
// modules of the microservice; consequently, any change
// in the configuration will be valid for all!

// Setting console transport (this will replace the console set by default)
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

// Instantiate a logger for the main application
// No package/custom name is given, so it'll get
// the package name 'sample-logging-app' from the
// package.json.
const logger = new Logger();
logger.info('Started sample logging application ...');

setInterval(() => {
  logger.info('Changing logging verbose mode ...');
  Logger.setVerbose(!Logger.getVerbose());
  logger.info('Logging sample messages ...');
  logSampleMessages();
}, 5000);
