import { mockDeep } from 'jest-mock-extended'
import { WebUtils } from '@dojot/microservice-sdk'

import { KafkaConsumer, TenantManager } from 'src/kafka'

export const KafkaMock = {
  new(tenants: WebUtils.TenantInfo[] = []) {
    const KafkaConsumerMock = mockDeep<KafkaConsumer>()
    const TenantManagerMock = mockDeep<TenantManager>({ tenants })

    return {
      KafkaConsumerMock,
      TenantManagerMock,
    }
  },
}
