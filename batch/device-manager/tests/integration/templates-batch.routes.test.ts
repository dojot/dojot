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
import { RemoveTemplatesBatchDto } from 'src/types';

jest.mock('@prisma/client', () => ({
  PrismaClient: jest.fn().mockImplementation(() => ({
    $disconnect: jest.fn(),
    templates: {
      delete: jest.fn().mockResolvedValue({}),
      findMany: jest.fn().mockResolvedValue({}),
    },
  })),
}));

describe('Templates-batch.routes', () => {
  const removeTemplatesBatchDto: RemoveTemplatesBatchDto = {
    templates: [1, 2, 8],
  };

  it('should return response status 200 and ids and label of templates removed ', async () => {
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
      .put('/templates_batch')
      .set('Authorization', `Bearer ${AuthSetup.signJWT()}`)
      .send(removeTemplatesBatchDto);

    server.close();
    expect(response.status).toBe(200);
  });

  it('should return response status 400 with validation field', async () => {
    const removeTemplatesBatchDto: RemoveTemplatesBatchDto = {
      templates: [],
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
      .put('/templates_batch')
      .set('Authorization', `Bearer ${AuthSetup.signJWT()}`)
      .send(removeTemplatesBatchDto);

    server.close();
    expect(response.status).toBe(400);
  });
});
