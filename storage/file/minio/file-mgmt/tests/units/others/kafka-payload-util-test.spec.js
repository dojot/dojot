const KafkaPayloadUtil = require('../../../src/utils/kafka-payload-util');

describe('KafkaPayloadUtil', () => {
  it('Should pass validation', async () => {
    const payload = {
      value: Buffer.from('{ "value": 1 }'),
    };

    const value = KafkaPayloadUtil.getValue(payload);
    expect(value).toEqual({ value: 1 });
  });
});
