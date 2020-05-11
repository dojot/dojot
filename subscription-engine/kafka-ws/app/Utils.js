/* Constants */

// Supported escaped chars
const escapeChars = {
  b: '\b',
  f: '\f',
  n: '\n',
  r: '\r',
  t: '\t',
};

// All supported operators
const operators = ['gt', 'gte', 'lt', 'lte', 'eq', 'neq', 'in', 'nin'];

// Numeric operators
const numericOperators = ['gt', 'gte', 'lt', 'lte', 'eq', 'neq'];

// Set operators, can be seem as string operators too
const setOperators = ['in', 'nin'];

/* Functions */

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

module.exports = {
  escapeChars,
  numericOperators,
  operators,
  setOperators,
  isNumber,
};
