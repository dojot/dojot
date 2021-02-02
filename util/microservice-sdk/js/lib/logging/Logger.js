/**
 * @overview Wrapper over Winston Logger to be used with dojot microservices.
 *
 */
const winston = require('winston');
const { addFileAndLineToMetadata, getRootPackageName } = require('./Utils');
const { sdkLevels, isLevelValid } = require('./Levels');
const { isTransportValid, createWinstonTransport } = require('./Transports');

// Logger wrapper over the shared winston logger instance
class Logger {
  /**
   * Constructor.
   * Creates a child logger for the given service/module from
   * a shared winston logger instance.
   *
   * @param { string } sid service/module identification. If not given,
   * it'll try to get from the root package.json.
   */
  constructor(sid) {
    let tmpSid = sid;

    // if not defined, it'll try to get from the package.json,
    // otherwise, it'll validate whether the given string is not empty
    if (typeof tmpSid === 'undefined') {
      tmpSid = getRootPackageName();
      if (!tmpSid) {
        throw new Error('Cannot discover the package name.');
      }
    } else if (typeof tmpSid !== 'string') {
      throw new Error('The sid must be a string value.');
    } else if (tmpSid === '') {
      throw new Error('The sid must be a non-empty string.');
    }

    // creates a child logger
    this.logger = Logger.sharedLogger.wlogger.child({ sid: tmpSid });
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
    let levelLower;

    // validate parameters
    if (!isTransportValid(transport)) {
      throw new Error('The transport value is not valid.');
    }
    // force lower case for logging level
    try {
      levelLower = level.toLowerCase();
    } catch (error) {
      throw new Error('The level value is not valid.');
    }
    if (!isLevelValid(levelLower)) {
      throw new Error('The level value is not valid.');
    }

    // set level
    if (Logger.sharedLogger.transports[transport]) {
      Logger.sharedLogger.transports[transport].level = levelLower;
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
    if (Logger.sharedLogger.transports[transport]) {
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
    Logger.sharedLogger.transports[transport] = newTransport;
    Logger.sharedLogger.wlogger.add(Logger.sharedLogger.transports[transport]);
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
    if (Logger.sharedLogger.transports[transport]) {
      Logger.sharedLogger.wlogger.remove(Logger.sharedLogger.transports[transport]);
      Logger.sharedLogger.transports[transport] = null;
    }
  }

  /**
   * Enables/Disables verbose mode.
   * In verbose mode, file and line of where the logging method has been
   * called are added as metadata to the logging message. It should be
   * avoid in production once it's time and resource consuming.
   * Only enable verbose mode for debugging purposes.
   *
   * @param {*} enable
   */
  static setVerbose(enable) {
    // validate parameters
    if (typeof enable !== 'boolean') {
      throw new Error('The parameter enable must be a boolean.');
    }

    Logger.sharedLogger.verbose = enable;
  }

  /**
   * Gets whether the verbose mode is enabled or not.
   *
   * @return { boolean } true whether the verbose mode is enabled,
   * otherwise, false.
   */
  static getVerbose() {
    return Logger.sharedLogger.verbose;
  }

  /**
   *  Writes an error logging message to the pre-defined transports.
   *
   * @param { string } message the logging message.
   * @param { object } metadata additional information to be included
   * in the final logging message (optional).
   */
  error(message, metadata = {}) {
    return this.logger.error(message, Logger.sharedLogger.handleMetadata(metadata));
  }

  /**
   *  Writes a warning logging message to the pre-defined transports.
   *
   * @param { string } message the logging message.
   * @param { object } metadata additional information to be included
   * in the final logging message (optional).
   */
  warn(message, metadata = {}) {
    return this.logger.warn(message, Logger.sharedLogger.handleMetadata(metadata));
  }

  /**
   *  Writes an information logging message to the pre-defined transports.
   *
   * @param { string } message the logging message.
   * @param { object } metadata additional information to be included
   * in the final logging message (optional).
   */
  info(message, metadata = {}) {
    return this.logger.info(message, Logger.sharedLogger.handleMetadata(metadata));
  }

  /**
   *  Writes a debug logging message to the pre-defined transports.
   *
   * @param { string } message the logging message.
   * @param { object } metadata additional information to be included
   * in the final logging message (optional).
   */
  debug(message, metadata = {}) {
    return this.logger.debug(message, Logger.sharedLogger.handleMetadata(metadata));
  }
}

// Internal winston logger shared by all instances of the logger wrapper.
// This could be a native static property of the wrapper, but es6 doesn't support that.
Logger.sharedLogger = {
  // Winston transports set to the logger
  transports: {
    console: null,
    file: null,
  },
  // Winston logger
  wlogger: winston.createLogger({
    // exitOnError: if false, handled exceptions will not cause process.exit
    exitOnError: false,
    // custom levels: error, warn, info, debug
    levels: sdkLevels.levels,
  }),
  // true to add file/line metadata to the logging messages, otherwise, false.
  verbose: true,
  // handle metadata
  handleMetadata: (metadata) => (Logger.sharedLogger.verbose
    ? addFileAndLineToMetadata(metadata) : metadata),
};

module.exports = { Logger };
