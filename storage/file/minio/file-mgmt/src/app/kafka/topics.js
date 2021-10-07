const KafkaController = require('./controllers/kafka-controller');

const topics = (config) => {
  const kafkaController = new KafkaController(null);

  return [
    {
      topicSuffix: config.subscribe['topics.suffix.tenants'],
      handler: kafkaController.createTenant,
    },
  ];
};

module.exports = topics;
