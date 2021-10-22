const KafkaPayloadUtil = require('../../../utils/kafka-payload-util');

class KafkaController {
  constructor(tenantService, logger) {
    this.tenantService = tenantService;
    this.logger = logger;
  }

  handle = async (payload, ack) => {
    try {
      const data = KafkaPayloadUtil.getValue(payload);
      const operation = {
        CREATE: this.tenantService.create,
      };

      this.logger.info(`Creating bucket for ${data.tenant} tenant`);
      await operation[data.type](data.tenant);
      ack();
    } catch (error) {
      this.logger.error(error);
    }
  }
}

module.exports = KafkaController;
