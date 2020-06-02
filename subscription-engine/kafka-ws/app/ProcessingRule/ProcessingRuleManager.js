const { Logger } = require('@dojot/microservice-sdk');

const { processingRule } = require('./ProcessingRule');
const { createFingerprint } = require('../Utils');

const logger = new Logger();

/**
 * Manages ProcessingRule instances.
 */
class ProcessingRuleManager {
  constructor() {
    this.rules = {};

    return Object.seal(this);
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
  addRule(fields, conditions, kafkaTopic) {
    const fingerprint = createFingerprint(kafkaTopic, fields, conditions);
    logger.debug(`Topic: ${kafkaTopic}`);

    if (this.rules[fingerprint]) {
      this.rules[fingerprint] = Object.assign(
        this.rules[fingerprint], { count: this.rules[fingerprint].count + 1 },
      );
      return { rule: this.rules[fingerprint].rule, fingerprint };
    }

    const rule = processingRule(fields, conditions);

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
        this.rules[fingerprint] = Object.assign(
          this.rules[fingerprint], { count: this.rules[fingerprint].count - 1 },
        );
      } else {
        delete this.rules[fingerprint];
      }
    }
  }

  /**
   * Check whether has the rule
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
