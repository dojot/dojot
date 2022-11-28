import { ServiceStateManager } from '@dojot/microservice-sdk'
import { mockDeep } from 'jest-mock-extended'

export const ServiceStateMock = {
  new() {
    return mockDeep<ServiceStateManager>()
  },
}
