import { mockDeep } from 'jest-mock-extended'
import { WebUtils } from '@dojot/microservice-sdk'

export const KeycloakClientSessionMock = {
  new() {
    return mockDeep<WebUtils.KeycloakClientSession>()
  },
}
