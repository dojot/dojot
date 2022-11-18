import { KafkaUtils } from '../../../src/utils/kafka.utils';

describe('Kafka.utils', () => {
  const kafkaUtils = new KafkaUtils();

  it('should parse the value', () => {
    expect(kafkaUtils.getValue({ value: '{ "tenant": "admin" }' })).toEqual({
      tenant: 'admin',
    });
  });
});
