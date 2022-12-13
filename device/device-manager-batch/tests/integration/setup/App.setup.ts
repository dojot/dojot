import { App } from 'src/app';
import {
  AppMock,
  KafkaMock,
  LoggerMock,
  ConfigMock,
  ServiceStateMock,
} from 'tests/mocks';

import { AuthSetup } from '.';

/**
 * Steps to improve integration tests in the future:
 * - Mock kafka but not KafkaConsumer and not TenantManager
 * - Try yo use SQLite instead of mocking the PrismaClient
 * - Try to mock Redis instead of mocking Bull
 *
 * Basically:
 * Try to reduce the number of mocks in integration tests.
 */

export const initApp = async () => {
  const { PrismaUtilsMock } = AppMock.new();

  const { KafkaConsumerMock, TenantManagerMock, KafkaProducerMock } =
    KafkaMock.new([AuthSetup.getTenantInfo()]);

  const app = new App(
    LoggerMock.new(),
    ConfigMock.new(),
    PrismaUtilsMock,
    KafkaConsumerMock,
    TenantManagerMock,
    KafkaProducerMock,
    ServiceStateMock.new(),
  );

  return app.init();
};
