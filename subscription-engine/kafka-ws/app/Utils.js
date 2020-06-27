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

// All supported operators
const operators = numericOperators.concat(setOperators);

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
function createFingerprint(topic, fields, conditions) {
  let sortedFields;

  if (fields) {
    sortedFields = fields.split(',').sort().toString();
  }

  const sortedConditions = conditions.map(
    (condition) => convertConditionToString(condition),
  ).sort().join('');

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
 * Checks if a value is a number (integer or float).
 *
 * @param {string} value
 *
 * @returns {boolean} true if it is a number, false otherwise
 */
function isNumber(value) {
  // Integer
  if (value.match(/^-?[1-9][0-9]*$/)) return true;
  // Float with values before and after the dot
  if (value.match(/^-?[1-9][0-9]*\.[0-9]*$/)) return true;
  // Float with values only after the dot
  if (value.match(/^-?0?\.[0-9]+$/)) return true;

  return false;
}

/**
 * Checks if the given object is empty, like {}
 *
 * @param {object} object
 * @returns {boolean} true if it is empty
 */
const isObjectEmpty = (object) => (typeof object !== 'object') || (object !== null && Object.keys(object).length === 0);

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

module.exports = {
  escapeChars,
  numericOperators,
  operators,
  setOperators,
  createFingerprint,
  isNumber,
  isObjectEmpty,
  checkTopicBelongsTenant,
  addTimeFromNow,
};
