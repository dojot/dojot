/**
 * @overview Logging transports for being used with Winston Logger.
 */
const winston = require('winston');
require('winston-daily-rotate-file');
const { textFormat, jsonFormat } = require('./Formats');

// console transport
// for more details see:
// https://github.com/winstonjs/winston/blob/HEAD/docs/transports.md#console-transport
const defaultConsoleConfig = {
  level: 'info',
  format: textFormat,
};

// file transport
// for more details see:
// https://github.com/winstonjs/winston-daily-rotate-file#readme
const defaultFileConfig = {
  level: 'info',
  dirname: '/var/log/',
  filename: 'dojot.microservice-%DATE%.log',
  datePattern: 'YYYY-MM-DD',
  zippedArchive: true,
  maxSize: '10m',
  maxFiles: '7d',
  format: jsonFormat,
};

/**
 * Checks whether the transport's name is valid.
 *
 * @param { string } transport name of the transport
 * @return true if the transport's name is valid; otherwise, false.
 */
function isTransportValid(transport) {
  return ((transport === 'console') || (transport === 'file'));
}

/**
 * Creates a transport for a winston logger.
 *
 * @param { string } name name of the transport. It can be:
 * 'console' or 'file'.
 * @param { object } config valid properties for the given transport.
 */
function createWinstonTransport(name, config) {
  let transport = null;

  switch (name) {
    case 'console': {
      const mergedConfig = Object.assign(defaultConsoleConfig, config);
      transport = new winston.transports.Console(mergedConfig);
      break;
    }
    case 'file': {
      const mergedConfig = Object.assign(defaultFileConfig, config);
      transport = new winston.transports.DailyRotateFile(mergedConfig);
      break;
    }
    default:
      break;
  }

  return transport;
}

module.exports = {
  isTransportValid,
  createWinstonTransport,
};
