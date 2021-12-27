const { processRule } = require('./ProcessingRule');
const { createFingerprint } = require('../Utils');

/**
 * Manages ProcessingRule instances.
 */
class ProcessingRuleManager {
  constructor() {
    this.rules = {};

    Object.seal(this);
  }

  /**
   * Creates a new ProcessingRule if there is no other rule with the same identification.
   *
   * @param {string} fields
   * @param {{parameter: string, operator: string, values: string[]}[]} conditions
   * @param {string} kafkaTopic
   *
   * @returns {{rule: (data: JSON) => JSON, fingerprint: string}}
   * function to process the data and its identification.
   */
  addRule(
    kafkaTopic, fields, conditions,
  ) {
    const fingerprint = createFingerprint(
      kafkaTopic, fields, conditions,
    );

    if (this.hasRule(fingerprint)) {
      this.rules[fingerprint] = Object
        .assign(this.rules[fingerprint], { count: this.rules[fingerprint].count + 1 });
      return { rule: this.rules[fingerprint].rule, fingerprint };
    }

    const rule = processRule(fields, conditions);

    this.rules[fingerprint] = { count: 1, rule };

    return { rule, fingerprint };
  }

  /**
   * Removes a rule from the list.
   *
   * @param {string} fingerprint rule identification
   */
  removeRule(fingerprint) {
    if (this.hasRule(fingerprint)) {
      if (this.rules[fingerprint].count > 1) {
        this.rules[fingerprint] = Object
          .assign(this.rules[fingerprint], { count: this.rules[fingerprint].count - 1 });
      } else {
        delete this.rules[fingerprint];
      }
    }
  }

  /**
   * Checks whether a ProcessingRule exists.
   *
   * @param {string} fingerprint rule identification
   *
   * @returns {bool} boolean indicating whether has the rule
   */
  hasRule(fingerprint) {
    return (!!this.rules[fingerprint]);
  }
}

module.exports = { ProcessingRuleManager };
