const flat = require('flat');

const Conditions = require('./Conditions');
const Utils = require('../Utils');

/**
 * A condition that can be applied to any given JSON. This is an abstraction to condition functions.
 *
 * @param {string} parameter parameter to apply the condition
 * @param {string} operator operator to be applied
 * @param {[]} value values to apply the operation
 *
 * @returns {(data: JSON) => JSON} a filter function that can be applied to a JSON
 */
const ConditionApplier = (
  parameter, operator, values,
) => (data) => {
  let filtered = flat(data);

  // The set operations accept multiple values, so we pass an array
  if (Utils.setOperators.includes(operator)) {
    filtered = Conditions[operator](
      parameter, values, filtered,
    );
  // The other operations accept only one value, so we pass the first value (a string)
  } else {
    filtered = Conditions[operator](
      parameter, values[0], filtered,
    );
  }

  return flat.unflatten(filtered);
};

module.exports = { ConditionApplier };
