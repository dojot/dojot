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
        CREATE: this.handleCreate.bind(this),
        DELETE: this.handleDelete.bind(this),
      };

      this.logger.info(`${data.type} bucket for ${data.tenant} tenant`);
      await operation[data.type](data.tenant);
      ack();
    } catch (error) {
      this.logger.error(error);
      ack(error);
    }
  }

  async handleCreate(payload) {
    await this.tenantService.create({
      id: payload.tenant,
      sigKey: {
        certificate: payload.certificate,
      },
    });
  }

  async handleDelete(payload) {
    await this.tenantService.remove(payload.tenant);
  }
}

module.exports = KafkaController;
