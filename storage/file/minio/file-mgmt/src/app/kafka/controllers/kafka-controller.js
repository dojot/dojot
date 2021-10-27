const KafkaPayloadUtil = require('../../../utils/kafka-payload-util');

/**
 * Responsible for controlling kafka's operations
 *
 * @class
 */
class KafkaController {
  constructor(tenantService, logger) {
    this.tenantService = tenantService;
    this.logger = logger;
  }

  /**
   * Handles location topic
   *
   * @param {*} payload Kafka payload
   * @param {*} ack ack callback function
   */
  handleTenancy = async (payload, ack) => {
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
