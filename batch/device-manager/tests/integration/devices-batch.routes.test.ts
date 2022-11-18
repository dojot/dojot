import request from 'supertest';

import {
  AppMock,
  KafkaMock,
  ConfigMock,
  LoggerMock,
  ServiceStateMock,
} from '../mocks';
import { App } from '../../src/app';
import { RemoveDevicesBatchDto } from '../../src/app/dto/remove-devices-batch.dto';
import { AuthSetup } from './setup';

jest.mock('@prisma/client', () => ({
  PrismaClient: jest.fn().mockImplementation(() => ({
    $disconnect: jest.fn(),
    devices: {
      delete: jest.fn().mockResolvedValue({}),
    },
  })),
}));

describe('Devices-batch.routes', () => {
  const removeDevicesBatchDto: RemoveDevicesBatchDto = {
    devices: ['1', '2'],
  };

  it('should return response status 200 and ids and label of devices removed', async () => {
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
    KafkaProducerMock.isConnected.mockResolvedValue(true);
    const server = await app.init();

    const response = await request(server)
      .put('/devices_batch')
      .set('Authorization', `Bearer ${AuthSetup.signJWT()}`)
      .send(removeDevicesBatchDto);

    server.close();
    expect(response.status).toBe(200);
  });

  it('should return response status 400 with validation field', async () => {
    const removeDevicesBatchDto: RemoveDevicesBatchDto = {
      devices: [],
    };
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
    KafkaProducerMock.isConnected.mockResolvedValue(true);
    const server = await app.init();

    const response = await request(server)
      .put('/devices_batch')
      .set('Authorization', `Bearer ${AuthSetup.signJWT()}`)
      .send(removeDevicesBatchDto);

    server.close();
    expect(response.status).toBe(400);
  });
});
