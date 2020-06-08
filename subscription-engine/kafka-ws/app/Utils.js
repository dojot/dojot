const Crypto = require('crypto-js');
const url = require('url');
const { pathToRegexp } = require('path-to-regexp');

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

  let fingerprint = topic;

  if (sortedFields) {
    fingerprint += `&${sortedFields}`;
  }
  if (sortedConditions) {
    fingerprint += `&${sortedConditions}`;
  }

  return Crypto.SHA1(fingerprint).toString();
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
 * Check if object if empty, like {}
 *
 * @param {object} object
 * @returns {boolean} true if it is empty
 */
const isObjectEmpty = (object) => Object.keys(object).length === 0;

/**
 * Parse JWT and get expirationTimestamp (sec) and tenant
 * @param {string} rawToken
 *
 * @returns {object} obj like {expirationTimestamp: 1591638638 , tenant: 'example'}
 */
const parseTenantAndExpTimeFromToken = (rawToken) => {
  if (!rawToken) {
    throw new Error('There is no authorization token in the header');
  }

  const tokenSplit = rawToken.split('.');

  if (tokenSplit.length !== 3) {
    throw new Error('Invalid token');
  }

  const tokenData = JSON.parse((Buffer.from(tokenSplit[1], 'base64')).toString());
  const { service: tenant, exp: expirationTimeSec } = tokenData;

  if (!tenant) {
    throw new Error('Tentant is not inside the token.');
  }
  if (!expirationTimeSec) {
    throw new Error('Expiration Time is not inside the token.');
  }

  return {
    tenant,
    expirationTimestamp: expirationTimeSec,
  };
};

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
 * Parse parthname from a URL in substring matches
 *
 * @param {string} fullUrl ex: http://google.com
 * @param {string} pathToRegex ex: /v1/websocket/:topic
 *
 * @returns {array} substring matches
 */
const checkAndParseURLPathname = (fullUrl, pathToRegex) => {
  const { pathname } = url.parse(fullUrl);
  const regexpRoute = pathToRegexp(pathToRegex);
  const parsedRoute = regexpRoute.exec(pathname);
  if (!parsedRoute) {
    throw new Error('Malformed Pathname');
  }
  return parsedRoute;
};

/**
 * adds seconds to the current timestamp and returns in seconds
 *
 * @param {number} addSec
 */
const timestampNowAddSeconds = (addSec) => Math.round(new Date().getTime() / 1000)
  + addSec;

module.exports = {
  escapeChars,
  numericOperators,
  operators,
  setOperators,
  createFingerprint,
  isNumber,
  isObjectEmpty,
  parseTenantAndExpTimeFromToken,
  checkTopicBelongsTenant,
  checkAndParseURLPathname,
  timestampNowAddSeconds,
};
