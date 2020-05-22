const curryRight = require('lodash/curryRight');
const isEmpty = require('lodash/isEmpty');
const mask = require('json-mask');

const { ConditionApplier } = require('../Conditions');

/**
 * A processing rule that can be applied to any given JSON.
 *
 * @param {string} fields
 * @param {{parameter: string, operator: string, values: string[]}[]} conditions
 *
 * @returns {(data: Object) => Object} a filter function that can be applied to a JSON
 */
const processingRule = (fields, conditions) => {
  const filters = [];

  conditions.forEach((condition) => {
    filters.push(ConditionApplier(condition.parameter, condition.operator, condition.values));
  });

  /* A filter must be a function that follows the pattern:
   * (data: JSON) => JSON
   */

  // Storing fields filter
  /*
   * The `mask` function removes all fields that are not in the variable `fields`. Check json-mask
   * NPM package for more info.
   *
   * The curryRight function transforms the mask function to an arity 1 curried function.
   * A curried function is a function that receives (ideally) one parameter and returns another
   * function. Example of a curried function:
   * let add = (a) => (b) => a + b;
   * We call it this way:
   * add(1)(3)
   * The curryRight function transforms a non-curried function into a curried one, but it inverts
   * the parameters order, i.e., the last parameter will be the first to be passed.
   */
  filters.push(curryRight(mask)(fields));

  // Applying the filters when a JSON is passed
  return (data) => {
    let filtered = data;

    filters.forEach((filter) => {
      if (isEmpty(filtered)) {
        return;
      }
      filtered = filter(filtered);
    });

    return filtered;
  };
};

module.exports = { processingRule };
