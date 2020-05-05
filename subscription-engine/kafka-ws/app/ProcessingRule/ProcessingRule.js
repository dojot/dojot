const curryRight = require('lodash/curryRight');
const mask = require('json-mask');

const { ConditionApplier } = require('../Conditions');

/**
 * A processing rule that can be applied to any given JSON.
 *
 * @param {string} fields
 * @param {{parameters: string, operator: string, values: string}[]} conditions
 *
 * @returns {(data: JSON) => JSON} a filter function that can be applied to a JSON
 */
const ProcessingRule = (fields, conditions) => {
  const filters = [];

  /* A filter must be a function that follows the pattern:
   * (data: JSON) => JSON
   */

  // Storing fields filter
  filters.push(curryRight(mask)(fields));

  conditions.forEach((condition) => {
    filters.push(ConditionApplier(condition.parameter, condition.operator, condition.values));
  });

  // Applying the filters when a JSON is passed
  return (data) => {
    let filtered = data;

    filters.forEach((filter) => {
      if (filtered) {
        filtered = filter(filtered);
      }
    });

    return filtered;
  };
};

module.exports = { ProcessingRule };
