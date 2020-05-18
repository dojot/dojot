// const { Kafka: { Consumer } } = require('@dojot/microservice-sdk');
const { logger } = require('@dojot/dojot-module-logger');

const { ProcessingRule } = require('./ProcessingRule');
const { createFingerprint } = require('../Utils');


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

    const rule = ProcessingRule(fields, conditions);
    const consumer = null;
    // TODO
    // const consumer = new Consumer({
    //   'group.id': kafkaTopic,
    //   'metadata.broker.list': `${Config.kafka.host}:${Config.kafka.port}`,
    //   'auto.offset.reset': 'beginning',
    // });
    this.rules[fingerprint] = { count: 1, rule, consumer };

    return { rule, fingerprint };
  }

  /**
   * Removes a rule from the list.
   *
   * @param {string} fingerprint rule identification
   */
  removeRule(fingerprint) {
    if (this.rules[fingerprint]) {
      if (this.rules[fingerprint].count > 1) {
        this.rules[fingerprint] = Object.assign(
          this.rules[fingerprint], { count: this.rules[fingerprint].count - 1 },
        );
      } else {
        delete this.rules[fingerprint];
      }
    }
  }
}

module.exports = { ProcessingRuleManager };
