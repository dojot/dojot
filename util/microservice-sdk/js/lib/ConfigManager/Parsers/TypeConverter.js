/**
 * Converts to boolean.
 *
 * @param {string} value
 *
 * @returns {boolean}
 */
const toBoolean = (value) => value.toString().trim().toLowerCase() === 'true';

/**
 * Converts to float.
 *
 * @param {string} value
 *
 * @returns {number}
 */
const toFloat = (value) => Number(value);

/**
 * Converts to integer.
 *
 * @param {string} value
 *
 * @returns {number}
 */
const toInteger = (value) => parseInt(value, 10);

/**
 * Converts to string.
 *
 * @param {string} value
 *
 * @returns {string}
 */
const toString = (value) => value.toString().trim();

/**
 * Converts to string[].
 *
 * @param {string} value
 *
 * @returns {string[]}
 */
const toStringArray = (value) => {
  const stringArray = value.toString()
    // Removing [ from the beginning and ] from the end
    .slice(1, -1)
    .split(',')
    // Removing " from the beginning and the end
    .map((str) => str.trim().slice(1, -1));

  return stringArray;
};

module.exports = {
  boolean: toBoolean,
  float: toFloat,
  integer: toInteger,
  string: toString,
  'string[]': toStringArray,
};
