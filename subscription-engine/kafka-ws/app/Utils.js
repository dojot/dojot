const Crypto = require('crypto-js');

/* Constants */

// Supported escaped chars
const escapeChars = {
  b: '\b',
  f: '\f',
  n: '\n',
  r: '\r',
  t: '\t',
};

// Numeric operators
const numericOperators = ['gt', 'gte', 'lt', 'lte', 'eq', 'neq'];
// Set operators, can be seem as string operators too
const setOperators = ['in', 'nin'];
// Boolean operators
const booleanOperators = ['bool'];

// All supported operators
const operators = numericOperators.concat(setOperators, booleanOperators);

/* Functions */

/**
 * Converts a condition to its canonical string form. In this form, the `values` are sorted and the
 * `equal` operator is displayed. The format is:
 * ```
 * <parameter>=<operator>:<values>;
 * ```
 *
 * @param {{parameter: string, operator: string, values: string[]}} condition
 *
 * @returns {string}
 */
function convertConditionToString(condition) {
  return `${condition.parameter}=${condition.operator}:${condition.values.sort().join(',')};`;
}

/**
 * Creates the fingerprint for a processing rule.
 *
 * @param {string} topic Kafka topic
 * @param {string} fields
 * @param {{parameter: string, operator: string, values: string[]}[]} conditions
 *
 * @returns {string}
 */
function createFingerprint(
  topic, fields, conditions,
) {
  let sortedFields;

  if (fields) {
    sortedFields = fields.split(',').sort().toString();
  }

  const sortedConditions = conditions.map((condition) => convertConditionToString(condition)).sort().join('');

  let processingRule = topic;

  if (sortedFields) {
    processingRule += `&${sortedFields}`;
  }
  if (sortedConditions) {
    processingRule += `&${sortedConditions}`;
  }

  return Crypto.SHA1(processingRule).toString();
}

/**
 * Checks if the topic belongs to tenant,
 *  for that the topic must be started with {tenant}.
 *
 * @param {string} topic
 * @param {string} tenant
 *
 * @returns {boolean} true if the topic starts with {tenant}.
 */
const checkTopicBelongsTenant = (topic, tenant) => {
  if (topic && tenant) {
    return (topic
      .toLowerCase()
      .startsWith(`${tenant.toLowerCase()}.`));
  }
  return false;
};

/**
 * Adds a given amount of seconds to the current time, and returns it in seconds.
 *
 * @param {number} addSec
 */
const addTimeFromNow = (addSec) => Math.round(new Date().getTime() / 1000)
  + addSec;

/* parse unsigned integer */
const parseUint = (val) => val && Math.abs(parseInt(val, 10));

/**
 * Converts the value to boolean. Accepted values for a boolean:
 * - `true/false` (case insensitive)
 * - `0/1`
 *
 * Any value or type that differs from these will throw an error.
 *
 * @param {string | number} val
 *
 * @throws {Error} when an invalid value is detected
 */
const parseBoolean = (value) => {
  if (value === null || value === undefined || Number.isNaN(value) || value === '') {
    throw new Error('invalid value for a boolean');
  }

  const stringifiedValue = value.toString().toLowerCase().trim();
  if (/^(true|1)$/.test(stringifiedValue)) return true;
  if (/^(false|0)$/.test(stringifiedValue)) return false;

  throw new Error('invalid value for a boolean');
};

/* parse array */
const parseArray = (val) => {
  if (typeof val !== 'string') return undefined;
  return val.split(',').map((el) => el.trim());
};

/**
 * Checks if a value is a boolean.
 *
 * @param {string | number} value
 *
 * @returns {boolean} true if it is a boolean, false otherwise
 */
function isBoolean(value) {
  try {
    parseBoolean(value);
    return true;
  } catch (error) {
    return false;
  }
}

/**
 * Checks if a value is a number (integer or float).
 *
 * @param {string} value
 *
 * @returns {boolean} true if it is a number, false otherwise
 */
const isNumber = (value) => {
  const number = Number(value);
  return !Number.isNaN(number) && Number.isFinite(number);
};

/**
 * Checks if the given object is empty, like {}
 *
 * @param {object} object
 * @returns {boolean} true if it is empty
 */
const isObjectEmpty = (object) => (typeof object !== 'object') || (object !== null && Object.keys(object).length === 0);

module.exports = {
  escapeChars,
  numericOperators,
  setOperators,
  booleanOperators,
  operators,
  createFingerprint,
  checkTopicBelongsTenant,
  addTimeFromNow,
  parseUint,
  parseBoolean,
  parseArray,
  isBoolean,
  isNumber,
  isObjectEmpty,
};
