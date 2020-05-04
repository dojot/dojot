const Consumer = require('./lib/kafka/Consumer');
const Producer = require('./lib/kafka/Producer');

module.exports = {
  Kafka: {
    Consumer,
    Producer,
  },
};
