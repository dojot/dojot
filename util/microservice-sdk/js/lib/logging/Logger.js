/**
 * @overview Wrapper over Winston Logger to be used with dojot microservices.
 *
 */
const winston = require('winston');
const { addFileAndLineToMetadata } = require('./Utils');
const { sdkLevels, isLevelValid } = require('./Levels');
const { isTransportValid, createWinstonTransport } = require('./Transports');

// Internal winston logger shared by all instances of the logger wrapper.
// This could be a static property of the wrapper, but es6 doesn't support that.
const wlogger = {
  // Winston transports set to the logger
  transports: {
    console: null,
    file: null,
  },
  // Winston logger
  logger: winston.createLogger({
    // exitOnError: if false, handled exceptions will not cause process.exit
    exitOnError: false,
    // custom levels: error, warn, info, debug
    levels: sdkLevels.levels,
  }),
};

// Logger wrapper over the shared winston logger instance
class Logger {
  /**
   * Constructor.
   * Creates a child logger for the given service/module from
   * a shared winston logger instance.
   *
   * @param { string } sid service/module identification.
   */
  constructor(sid) {
    // parameters validation
    if (typeof sid !== 'string') {
      throw new Error('The sid must be a string value.');
    }
    if (sid === '') {
      throw new Error('The sid must be a non-empty string.');
    }

    // creates a child logger
    this.logger = wlogger.logger.child({ sid });
  }

  /**
   * Sets the logging level for the given transport.
   * Note that this method is static, so it sets the 'global' logger.
   *
   * @param { string } transport  name of the transport. It can be:
   * 'console' or 'file'.
   * @param { string } level logging level. It can be: 'error', 'warn', 'info'
   * or 'debug'.
   */
  static setLevel(transport, level) {
    // validate parameters
    if (!isTransportValid(transport)) {
      throw new Error('The transport value is not valid.');
    }
    if (!isLevelValid(level)) {
      throw new Error('The level value is not valid.');
    }

    // set level
    if (wlogger.transports[transport]) {
      wlogger.transports[transport].level = level;
    } else {
      throw new Error('Transport has\'nt been set.');
    }
  }

  /**
   * Checks whether the given transport is set.
   *
   * @param { string } transport  name of the transport. It can be:
   * 'console' or 'file'.
   * @return { boolean } true whether the transport is set; otherwise, false.
   */
  static isTransportSet(transport) {
    // validate parameters
    if (!isTransportValid(transport)) {
      throw new Error('The transport value is not valid.');
    }

    // checks if it set
    if (wlogger.transports[transport]) {
      return true;
    }
    return false;
  }

  /**
   * Sets the transport.
   * Note that this method is static, so it sets the 'global' logger.
   *
   * @param { string } transport  name of the transport. It can be:
   * 'console' or 'file'.
   * @param { object } config valid properties for the given transport.
   * For more details see wtransports.js
   */
  static setTransport(transport, config = {}) {
    // validate parameters
    if (!isTransportValid(transport)) {
      throw new Error('The transport value is not valid.');
    }
    if (typeof config !== 'object') {
      throw new Error('The config must be an object value.');
    }

    // validate current state
    if (this.isTransportSet(transport)) {
      throw new Error('Transport has been set. It is necessary to unset it.');
    }

    // create the new  transport
    const newTransport = createWinstonTransport(transport, config);

    // set the new transport
    wlogger.transports[transport] = newTransport;
    wlogger.logger.add(wlogger.transports[transport]);
  }

  /**
   * Unsets the transport.
   * Note that this method is static, so it sets the 'global' logger.
   *
   * @param { string } transport  name of the transport. It can be:
   * 'console' or 'file'.
   */
  static unsetTransport(transport) {
    // validate parameters
    if (!isTransportValid(transport)) {
      throw new Error('The transport value is not valid.');
    }

    // remove existing transport
    if (wlogger.transports[transport]) {
      wlogger.logger.remove(wlogger.transports[transport]);
      wlogger.transports[transport] = null;
    }
  }

  /**
   *  Writes an error logging message to the pre-defined transports.
   *
   * @param { string } message the logging message.
   * @param { object } metadata additional information to be included
   * in the final logging message (optional).
   */
  error(message, metadata = {}) {
    return this.logger.error(message, metadata);
  }

  /**
   *  Writes a warning logging message to the pre-defined transports.
   *
   * @param { string } message the logging message.
   * @param { object } metadata additional information to be included
   * in the final logging message (optional).
   */
  warn(message, metadata = {}) {
    return this.logger.warn(message, metadata);
  }

  /**
   *  Writes an information logging message to the pre-defined transports.
   *
   * @param { string } message the logging message.
   * @param { object } metadata additional information to be included
   * in the final logging message (optional).
   */
  info(message, metadata = {}) {
    return this.logger.info(message, metadata);
  }

  /**
   *  Writes a debug logging message to the pre-defined transports.
   *
   * @param { string } message the logging message.
   * @param { object } metadata additional information to be included
   * in the final logging message (optional).
   */
  debug(message, metadata = {}) {
    return this.logger.debug(message, metadata);
  }

  /**
   *  Writes a verbose error logging message to the pre-defined transports.
   *  It adds to the final logging message the file and line where this
   *  method has been called. It should be avoid in production once it's
   *  time and resource consuming.
   *
   * @param { string } message the logging message.
   * @param { object } metadata additional information to be included
   * in the final logging message (optional).
   */
  errorv(message, metadata = {}) {
    return this.logger.error(message, addFileAndLineToMetadata(metadata));
  }

  /**
   *  Writes a verbose warning logging message to the pre-defined transports.
   *  It adds to the final logging message the file and line where this
   *  method has been called. It should be avoid in production once it's
   *  time and resource consuming.
   *
   * @param { string } message the logging message.
   * @param { object } metadata additional information to be included
   * in the final logging message (optional).
   */
  warnv(message, metadata = {}) {
    return this.logger.warn(message, addFileAndLineToMetadata(metadata));
  }

  /**
   *  Writes a verbose information logging message to the pre-defined transports.
   *  It adds to the final logging message the file and line where this
   *  method has been called. It should be avoid in production once it's
   *  time and resource consuming.
   *
   * @param { string } message the logging message.
   * @param { object } metadata additional information to be included
   * in the final logging message (optional).
   */
  infov(message, metadata = {}) {
    return this.logger.info(message, addFileAndLineToMetadata(metadata));
  }

  /**
   *  Writes a verbose debug logging message to the pre-defined transports.
   *  It adds to the final logging message the file and line where this
   *  method has been called. It should be avoid in production once it's
   *  time and resource consuming.
   *
   * @param { string } message the logging message.
   * @param { object } metadata additional information to be included
   * in the final logging message (optional).
   */
  debugv(message, metadata = {}) {
    return this.logger.debug(message, addFileAndLineToMetadata(metadata));
  }
}

module.exports = { Logger, wlogger };
