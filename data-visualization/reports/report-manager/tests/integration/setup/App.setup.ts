import { App } from 'src/app'
import {
  AppMock,
  KafkaMock,
  LoggerMock,
  ConfigMock,
  ServiceStateMock,
} from 'tests/mocks'

import { AuthSetup } from '.'

export const initApp = async () => {
  const { PrismaUtilsMock } = AppMock.new()

  const { KafkaConsumerMock, TenantManagerMock } = KafkaMock.new([
    AuthSetup.getTenantInfo(),
  ])

  const app = new App(
    LoggerMock.new(),
    ConfigMock.new(),
    PrismaUtilsMock,
    KafkaConsumerMock,
    TenantManagerMock,
    ServiceStateMock.new(),
  )

  return app.init()
}
