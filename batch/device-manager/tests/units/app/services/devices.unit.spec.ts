// Here the unit tests will be written.
import { describe } from '@jest/globals';
import { devices, templates } from '@prisma/client';
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

    const templates_fake: templates = {
      id: 1,
      label: 'modelo_1',
      created: new Date(),
      updated: new Date(),
    };

    const devices_fake: devices = {
      id: '1',
      label: 'dev1',
      created: new Date(),
      updated: new Date(),
      persistence: null,
    };

    const devices_associate_templates_and_attrs = [
      {
        id: '3',
        label: 'dev3',
        created: new Date(),
        updated: new Date(),
        persistence: null,
        device_template: [
          {
            templates: {
              id: 1,
              label: 'modelo1',
              created: new Date(),
              updated: new Date(),
              attrs: [
                {
                  id: 1,
                  label: 'teste_attr',
                  created: new Date(),
                  updated: new Date(),
                  type: 'static',
                  value_type: 'integer',
                  static_value: '10',
                  template_id: '1',
                },
              ],
            },
          },
        ],
      },
    ];
    it('should remove one id and retrun id, label of objetc removed.', async () => {
      const { KafkaProducerMock } = KafkaMock.new();
      const FakePrismaClient = PrismaClientMock.new();
      const devicesRepository = new DevicesRepository(LoggerMock.new());
      const templatesRepository = new TemplatesRepository(LoggerMock.new());
      const prismaUtils = new PrismaUtils(LoggerMock.new(), ConfigMock.new());

      const devicesServices = new DevicesServices(
        LoggerMock.new(),
        devicesRepository,
        KafkaProducerMock,
        prismaUtils,
        templatesRepository,
      );

      FakePrismaClient.devices.findUnique.mockResolvedValue(devices_fake);
      FakePrismaClient.devices.findMany.mockResolvedValue(
        devices_associate_templates_and_attrs,
      );
      FakePrismaClient.devices.delete.mockResolvedValue(devices_fake);
      const return_devices_removed = await devicesServices.remove(
        FakePrismaClient,
        removeDevicesBatchDto_fake,
        '',
      );
      expect(return_devices_removed.devices.length).toBe(1);
      expect(return_devices_removed.devices_not_found.length).toBe(0);
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
      );
      //FakePrismaClient.devices.findUnique.mockResolvedValue(devices_fake);
      const return_devices_created = await devicesServices.create(
        FakePrismaClient,
        createDevicesBatchDto_fake,
        '',
      );
    });
  });
});
