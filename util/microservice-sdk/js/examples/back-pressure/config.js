const config = {
  // Kafka's consumer group id
  'group.id': process.env.KAFKA_GROUP_ID || 'sdk-exampĺe',
  // Addresses of the kafka brokers separated by a comma
  'metadata.broker.list': process.env.KAFKA_HOSTS || 'kafka:9092',
};

module.exports = config;
