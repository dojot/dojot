/**
 * Manages the topic listeners.
 *
 * @param {*} config Application Settings
 * @param {*} controllers Kafka controllers
 *
 * @returns the topic listeners
 */
const topics = (config, controllers) => [
  {
    topicRegex: config.subscribe['topics.regex.tenants'],
    handler: controllers.kafkaController.handleTenancy,
  },
];

module.exports = topics;
