// Here the unit tests will be written.
import { describe } from '@jest/globals';
import { devices } from '@prisma/client';
import { RemoveDevicesBatchDto } from 'src/types';
import { DevicesServices } from '../../../../src/app/services/devicesServices';
import {
  ConfigMock,
  KafkaMock,
  LoggerMock,
  PrismaClientMock,
} from '../../../mocks';
import {
  DevicesRepository,
  TemplatesRepository,
  AttrsRepository,
} from '../../../../src/app/repository/';
import { PrismaUtils } from '../../../../src/utils/Prisma.utils';
import { CreateDevicesBatchDto } from 'src/app/dto/create-devices-batch.dto';

describe('devicesServices', () => {
  describe('remove', () => {
    const removeDevicesBatchDto_fake: RemoveDevicesBatchDto = {
      devices: ['1'],
    };

    const devices_fake: devices = {
      id: '1',
      label: 'dev1',
      created: new Date(),
      updated: new Date(),
      persistence: null,
    };

    it('should remove one id and retrun id, label of objetc removed.', async () => {
      const { KafkaProducerMock } = KafkaMock.new();
      const FakePrismaClient = PrismaClientMock.new();
      const devicesRepository = new DevicesRepository(LoggerMock.new());
      const templatesRepository = new TemplatesRepository(LoggerMock.new());
      const attrsRepository = new AttrsRepository(LoggerMock.new());
      const prismaUtils = new PrismaUtils(LoggerMock.new(), ConfigMock.new());

      const devicesServices = new DevicesServices(
        LoggerMock.new(),
        devicesRepository,
        KafkaProducerMock,
        prismaUtils,
        templatesRepository,
        attrsRepository,
      );
      FakePrismaClient.devices.findUnique.mockResolvedValue(devices_fake);
      const return_devices_removed = await devicesServices.remove(
        FakePrismaClient,
        removeDevicesBatchDto_fake,
        '',
      );
      expect(return_devices_removed).toEqual({
        devices: [{ id: '1', label: 'dev1' }],
      });
    });
  });

  describe('create', () => {
    it('should create devices 10, return in the array 10 devices created, with this sequence teste-1,teste-2,..., teste-10', async () => {
      const createDevicesBatchDto_fake: CreateDevicesBatchDto = {
        name_prefix: 'teste',
        quantity: 10,
        start_sufix: 1,
        associate_certificates: false,
        templates: [1],
        attrs: [],
      };

      const { KafkaProducerMock } = KafkaMock.new();
      const FakePrismaClient = PrismaClientMock.new();
      const devicesRepository = new DevicesRepository(LoggerMock.new());
      const templatesRepository = new TemplatesRepository(LoggerMock.new());
      const attrsRepository = new AttrsRepository(LoggerMock.new());
      const prismaUtils = new PrismaUtils(LoggerMock.new(), ConfigMock.new());

      const devicesServices = new DevicesServices(
        LoggerMock.new(),
        devicesRepository,
        KafkaProducerMock,
        prismaUtils,
        templatesRepository,
        attrsRepository,
      );
      //FakePrismaClient.devices.findUnique.mockResolvedValue(devices_fake);
      const return_devices_created = await devicesServices.create(
        FakePrismaClient,
        createDevicesBatchDto_fake,
        '',
      );
    });

    it('should create devices in batch with 10 devices created, sequence teste-11,teste-12,teste-13..., teste-20', async () => {});
  });
});
