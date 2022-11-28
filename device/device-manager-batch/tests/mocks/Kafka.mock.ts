import { mockDeep } from 'jest-mock-extended';
import { WebUtils } from '@dojot/microservice-sdk';

import { KafkaConsumer, KafkaProducer, TenantManager } from '../../src/kafka';

export const KafkaMock = {
  new(tenants: WebUtils.TenantInfo[] = []) {
    const KafkaConsumerMock = mockDeep<KafkaConsumer>();
    const KafkaProducerMock = mockDeep<KafkaProducer>();
    const TenantManagerMock = mockDeep<TenantManager>({ tenants });

    return {
      KafkaConsumerMock,
      KafkaProducerMock,
      TenantManagerMock,
    };
  },
};
