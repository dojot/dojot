const Consumer = require('./lib/kafka/Consumer');
const Producer = require('./lib/kafka/Producer');
const { Logger } = require('./lib/logging/Logger');

module.exports = {
  Kafka: {
    Consumer,
    Producer,
  },
  Logger,
};
