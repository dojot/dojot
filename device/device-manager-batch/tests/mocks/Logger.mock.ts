import { mock } from 'jest-mock-extended'
import { Logger } from '@dojot/microservice-sdk'

export const LoggerMock = {
  new() {
    return mock<Logger>()
  },
}
