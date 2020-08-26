const Utils = require('../Utils');

/**
 * Applies the greater than operator.
 *
 * @param {string} parameter parameter that will be analysed
 * @param {string} value value to be compared
 * @param {object} data the data to be filtered
 *
 * @returns {object} filtered data.
 */
const applyGt = (parameter, value, data) => {
  if (data[parameter] && Number(data[parameter]) > Number(value)) {
    return data;
  }
  return {};
};

/**
 * Applies the greater than or equal operator.
 *
 * @param {string} parameter parameter that will be analysed
 * @param {string} value value to be compared
 * @param {object} data the data to be filtered
 *
 * @returns {object} filtered data.
 */
const applyGte = (parameter, value, data) => {
  if (data[parameter] && Number(data[parameter]) >= Number(value)) {
    return data;
  }
  return {};
};

/**
 * Applies the less than operator.
 *
 * @param {string} parameter parameter that will be analysed
 * @param {string} value value to be compared
 * @param {object} data the data to be filtered
 *
 * @returns {object} filtered data.
 */
const applyLt = (parameter, value, data) => {
  if (data[parameter] && Number(data[parameter]) < Number(value)) {
    return data;
  }
  return {};
};

/**
 * Applies the less than or equal operator.
 *
 * @param {string} parameter parameter that will be analysed
 * @param {string} value value to be compared
 * @param {object} data the data to be filtered
 *
 * @returns {object} filtered data.
 */
const applyLte = (parameter, value, data) => {
  if (data[parameter] && Number(data[parameter]) <= Number(value)) {
    return data;
  }
  return {};
};

/**
 * Applies the equal operator.
 *
 * @param {string} parameter parameter that will be analysed
 * @param {string} value value to be compared
 * @param {object} data the data to be filtered
 *
 * @returns {object} filtered data.
 */
const applyEq = (parameter, value, data) => {
  if (data[parameter] && Number(data[parameter]) === Number(value)) {
    return data;
  }

  return {};
};

/**
 * Applies the not equal operator.
 *
 * @param {string} parameter parameter that will be analysed
 * @param {string} value value to be compared
 * @param {object} data the data to be filtered
 *
 * @returns {object} filtered data.
 */
const applyNeq = (parameter, value, data) => {
  if (data[parameter] && Number(data[parameter]) !== Number(value)) {
    return data;
  }

  return {};
};

/**
 * Applies the in operator.
 *
 * @param {string} parameter parameter that will be analysed
 * @param {string[]} values values to be compared
 * @param {object} data the data to be filtered
 *
 * @returns {object} filtered data.
 */
const applyIn = (parameter, values, data) => {
  for (let i = 0; i < values.length; i += 1) {
    if (data[parameter] && data[parameter].toString().match(values[i])) {
      return data;
    }
  }

  return {};
};

/**
 * Applies the not in operator.
 *
 * @param {string} parameter parameter that will be analysed
 * @param {string[]} values values to be compared
 * @param {object} data the data to be filtered
 *
 * @returns {object} filtered data.
 */
const applyNin = (parameter, values, data) => {
  let i = 0;
  for (i = 0; i < values.length; i += 1) {
    if (data[parameter] && data[parameter].toString().match(values[i])) {
      break;
    }
  }

  if (i >= values.length) {
    return data;
  }

  return {};
};

/**
 * Applies the not equal operator.
 *
 * @param {string} parameter parameter that will be analysed
 * @param {string} value value to be compared
 * @param {object} data the data to be filtered
 *
 * @returns {object} filtered data.
 */
const applyBool = (parameter, value, data) => {
  if (Utils.parseBoolean(data[parameter]) === Utils.parseBoolean(value)) {
    return data;
  }

  return {};
};

module.exports = {
  gt: applyGt,
  gte: applyGte,
  lt: applyLt,
  lte: applyLte,
  eq: applyEq,
  neq: applyNeq,
  in: applyIn,
  nin: applyNin,
  bool: applyBool,
};
