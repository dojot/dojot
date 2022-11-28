import { mockDeep } from 'jest-mock-extended'
import { WebUtils } from '@dojot/microservice-sdk'

export const DojotHttpClientMock = {
  new() {
    return mockDeep<WebUtils.DojotHttpClient>();
  },
}
