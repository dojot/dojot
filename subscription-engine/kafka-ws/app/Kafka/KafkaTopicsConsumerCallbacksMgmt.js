const { logger } = require('@dojot/dojot-module-logger');
const util = require('util');
const KafkaWSConsumers = require('./KafkaConsumers');

const TAG = { filename: 'kafka-ws:app/Kafka/KafkaTopicsCallbacksMgmt' };

/**
* Manages n callbacks for a single kafka topic,
*/
class KafkaTopicsConsumerCallbacksMgmt {
  /**
    * Instance KafkaTopicsCallbacksMgmt
    */
  constructor() {
    logger.debug('constructor: Instance KafkaTopicsCallbacksMgmt', TAG);
    // only one callback by topic
    this.topic = new Map();
    this.kafka = new KafkaWSConsumers();
  }

  /**
   *  Inicialize Kafka Topics Callbacks Mgmt
   */
  async init() {
    try {
      logger.debug('init: Starting KafkaTopicsCallbacksMgmt', TAG);
      await this.kafka.init();
    } catch (error) {
      logger.error(`init: Error starting KafkaTopicsCallbacksMgmt ${error.stack}`, TAG);
      throw error;
    }
  }

  /**
   * Adds a callback to a topic.
   *
   * @param {string} topic Kafka topic to be consumed
   * @param {string} idCallback This id must be unique within a topic's callback list.
   * @param {function} callback
   */
  addCallback(topic, idCallback, callback) {
    logger.debug(`addCallbackToTopic: Topic=${topic} idCallback=${idCallback}`, TAG);

    if (this.topic.has(topic)) {
      const callbackMap = this.topic.get(topic);
      callbackMap.set(idCallback, callback);
      logger.debug(`addCallbackToTopic: All Registered Callbacks for Topic ${topic}: ${[...callbackMap]}`, TAG);
      this.topic.set(topic, callbackMap);
    } else {
      const callbackMap = new Map();
      callbackMap.set(idCallback, callback);
      this.topic.set(topic, callbackMap);
      logger.debug(`addCallbackToTopic: All Registered Callbacks for Topic ${topic}: ${[...callbackMap]}`, TAG);
    }

    if (!this.kafka.registeredCallbacks.has(topic)) {
      this.kafka.registerCallback(topic, this.createCallbackKafkaConsumer(topic));
    }
  }

  /**
   * Removes a callback to a topic.
   *
   * @param {string} topic Kafka topic to be consumed
   * @param {string} idCallback This id must be unique within a topic's callback list.
   */
  removeCallback(topic, idCallback) {
    logger.debug(`removeCallbackToTopic: Topic=${topic} idCallback=${idCallback}`, TAG);

    if (this.topic.has(topic)) {
      const callbackMap = this.topic.get(topic);
      callbackMap.delete(idCallback);
      // if you no longer have any callbacks associated with this topic,
      // remove the callback from the consumer.
      if (callbackMap.size === 0) {
        this.kafka.unregisterCallback(topic);
        this.topic.delete(topic);
      } else {
        this.topic.set(topic, callbackMap);
      }
    }
  }

  /**
   * Auxiliary method that is called when registering the topic in the consumer
   *
   * This method returns a callback to be registered in the consumer's topic,
   * and within it all callbacks associated with a topic are called.
   *
   * @private
   * @param {string} topic
   * @return {function} A callback to be associated with the kafka consumer
   */
  createCallbackKafkaConsumer(topic) {
    if (this.topic.has(topic)) {
      const callbackMap = this.topic.get(topic);
      return (data) => {
        try {
          const { value: payloadEncoded } = data;
          const payload = JSON.parse(payloadEncoded.toString());

          logger.debug(`createCallbackKafka: Topic=${topic} Payload=${util.inspect(payload, { depth: null })}`, TAG);

          callbackMap.forEach((callback) => {
            callback(payload);
          });
        } catch (error) {
          logger.error(`createCallbackKafka: Caught ${error.stack}`, TAG);
        }
      };
    }
    return null;
  }
}

module.exports = KafkaTopicsConsumerCallbacksMgmt;
