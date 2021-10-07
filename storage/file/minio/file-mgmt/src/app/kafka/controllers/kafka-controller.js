const KafkaPayloadUtil = require('../../../utils/Kafka-payload-util');

class KafkaController {
  constructor(tenantCreationService) {
    this.tenantCreationService = tenantCreationService;
  }

  // eslint-disable-next-line class-methods-use-this
  createTenant = (payload, ack) => {
    const data = KafkaPayloadUtil.getValue(payload);
    console.log(data);
    ack();
  }
}

module.exports = KafkaController;
