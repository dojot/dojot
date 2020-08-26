const Utils = require('../Utils');
const { InvalidOperatorArity, InvalidValue } = require('../Errors').Errors;

/**
 * Base checker for numeric operators.
 *
 * @param {string[]} values
 * @param {string} operator
 *
 * @returns {number}
 */
const baseNumericChecker = (values, operator) => {
  // No more than one value is permitted for a numeric operator
  if (values.length !== 1) {
    throw new InvalidOperatorArity(operator, values.length);
  }
  if (!Utils.isNumber(values[0])) {
    throw new InvalidValue(operator, values[0]);
  }

  return Number(values[0]);
};

/**
 * Base checker for set operators.
 *
 * @param {string[]} values
 * @param {string} operator
 *
 * @returns {string[]}
 */
const baseSetChecker = (values, operator) => values.map((value) => {
  try {
    return value.toString();
  } catch (error) {
    throw new InvalidValue(operator, value);
  }
});

/**
 * Base checker for boolean operators.
 *
 * @param {string[]} values
 * @param {string} operator
 *
 * @returns {boolean}
 */
const baseBooleanChecker = (values, operator) => {
  // No more than one value is permitted for a boolean operator
  if (values.length !== 1) {
    throw new InvalidOperatorArity(operator, values.length);
  }
  if (!Utils.isBoolean(values[0])) {
    throw new InvalidValue(operator, values[0]);
  }
  return Utils.parseBoolean(values[0]);
};

const checkGt = (values) => baseNumericChecker(values, 'gt');
const checkGte = (values) => baseNumericChecker(values, 'gte');
const checkLt = (values) => baseNumericChecker(values, 'lt');
const checkLte = (values) => baseNumericChecker(values, 'lte');
const checkEq = (values) => baseNumericChecker(values, 'eq');
const checkNeq = (values) => baseNumericChecker(values, 'neq');

const checkIn = (values) => baseSetChecker(values, 'in');
const checkNin = (values) => baseSetChecker(values, 'nin');

const checkBool = (values) => baseBooleanChecker(values, 'bool');

module.exports = {
  gt: checkGt,
  gte: checkGte,
  lt: checkLt,
  lte: checkLte,
  eq: checkEq,
  neq: checkNeq,
  in: checkIn,
  nin: checkNin,
  bool: checkBool,
};
