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
  const valueToString = value.toString().trim();
  // Verifying whether the array is correctly delimited by []
  if (!valueToString.match(/^\[.+\]$/)) {
    throw new Error('invalid array of strings');
  }
  // Removing [ from the beginning and ] from the end
  const stringArray = valueToString.slice(1, -1).split(',');

  // Verifying whether all strings are correctly delimited by ' or "
  if (!stringArray.every((str) => str.toString().trim().match(/^['"].+['"]$/))) {
    throw new Error('found an invalid value in the passed array of strings');
  }
  // Removing " from the beginning and the end
  const resultingArray = stringArray.map((str) => str.trim().slice(1, -1));

  return resultingArray;
};

module.exports = {
  boolean: toBoolean,
  float: toFloat,
  integer: toInteger,
  string: toString,
  'string[]': toStringArray,
};
