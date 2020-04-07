const ConsumerBackPressure = require('./lib/kafka/ConsumerBackPressure');
const Producer = require('./lib/kafka/Producer');

module.exports = {
  Kafka: {
    ConsumerBackPressure,
    Producer,
  },
};
