/**
 * @overview Customized logging levels for being used with Winston Logger.
 */

/* This sdk will work with four levels of logging, wich are dividide into:
*
* error: This level serves as the general error feature. It should be used
* whenever the software encounters an unexpected error that prevents further
* processing (e.g. cant connect to a port, an error connecting with kafka, a
* connection refused).
*
* warn: Events that are likely to lead to an error in the future, however, can
* be corrected by the system runtime (e.g. a fail connection with database,
* fail trying to retrieve a data, fail trying to get a callback)
*
* info: System update information events (e.g A new socket connection, a new
* kafka producer).
*
* debug: Events for debug readings, usefull when developers are trying to
* understand the code (e.g. Kafka Producer is not yet ready,
* Retrieving/creating new topic). The level severity of logs can be changed
* via runtime by a http request into: ".../setLog?level={level of your
* debug}".
*/
const sdkLevels = {
  levels: {
    error: 0,
    warn: 1,
    info: 2,
    debug: 3,
  },
  colors: {
    error: 'red',
    warn: 'yellow',
    info: 'blue',
    debug: 'magenta',
  },
};

/**
 * Checks whether a level is valid.
 *
 * @param { string } level logging level
 * @return true if the logging level is valid; otherwise, false.
 */
function isLevelValid(level) {
  return Object.prototype.hasOwnProperty.call(sdkLevels.levels, level);
}

module.exports = {
  sdkLevels,
  isLevelValid,
};
