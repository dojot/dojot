const KafkaController = require('./controllers/kafka-controller');

const topics = (config, services, logger) => {
  const kafkaController = new KafkaController(services.tenantService, logger);

  return [
    {
      topicSuffix: config.subscribe['topics.suffix.tenants'],
      handler: kafkaController.handle,
    },
  ];
};

module.exports = topics;
