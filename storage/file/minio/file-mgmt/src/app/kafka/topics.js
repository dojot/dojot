const topics = (config, controllers) => [
  {
    topicSuffix: config.subscribe['topics.suffix.tenants'],
    handler: controllers.kafkaController.handle,
  },
];

module.exports = topics;
