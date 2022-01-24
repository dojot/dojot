const KafkaPayloadUtil = require('../../../utils/kafka-payload-util');

/**
 * Responsible for controlling kafka's operations.
 *
 * @class
 */
class KafkaController {
  constructor(tenantService, logger) {
    this.tenantService = tenantService;
    this.logger = logger;
  }

  /**
   * Handles location topic.
   *
   * @param {*} payload Kafka payload
   * @param {*} ack ack callback function
   */
  handleTenancy = async (payload, ack) => {
    try {
      const data = KafkaPayloadUtil.getValue(payload);
      const operation = {
        CREATE: this.tenantService.create,
        DELETE: this.tenantService.remove,
      };

      this.logger.info(`${data.type} bucket for ${data.tenant} tenant`);
      await operation[data.type](data.tenant);
      ack();
    } catch (error) {
      this.logger.error(error);
      ack(error);
    }
  }
}

module.exports = KafkaController;
