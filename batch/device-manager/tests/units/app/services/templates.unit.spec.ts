// Here the unit tests will be written.
import { describe } from '@jest/globals';
import { devices, device_template, templates } from '@prisma/client';
import { RemoveTemplatesBatchDto } from 'src/types';
import { TemplatesServices } from '../../../../src/app/services/templatesServices';
import { KafkaMock, LoggerMock, PrismaClientMock } from '../../../mocks';
import { TemplatesRepository } from '../../../../src/app/repository';

describe('TemplatesServices', () => {
  describe('remove', () => {
    const removeTemplatesBatchDto_fake: RemoveTemplatesBatchDto = {
      templates: [1],
    };

    const tenplates_fake1: templates = {
      id: 1,
      label: 'modelo1',
      created: new Date(),
      updated: new Date(),
    };

    const tenplates_fake2: templates = {
      id: 2,
      label: 'modelo2_associate',
      created: new Date(),
      updated: new Date(),
    };

    const devices_fake1: devices = {
      id: '1',
      label: 'dev1',
      created: new Date(),
      updated: new Date(),
      persistence: null,
    };

    const template_with_associate = [
      {
        id: 2,
        label: 'modelo1',
        created: new Date(),
        updated: new Date(),
        device_template: [
          {
            devices: {
              id: '1',
              label: 'dev1',
              created: new Date(),
              updated: new Date(),
              persistence: null,
            },
          },
        ],
      },
    ];

    const template_no_associate = [
      {
        id: 1,
        label: 'modelo1',
        created: new Date(),
        updated: new Date(),
        device_template: [],
      },
    ];

    it('should remove one id and retrun id, label of object in array of templates.', async () => {
      const removeTemplatesBatchDto_fake: RemoveTemplatesBatchDto = {
        templates: [1],
      };

      const { KafkaProducerMock } = KafkaMock.new();
      const FakePrismaClient = PrismaClientMock.new();
      const templatesRepository = new TemplatesRepository(LoggerMock.new());
      const templatesServices = new TemplatesServices(
        LoggerMock.new(),
        templatesRepository,
        KafkaProducerMock,
      );
      FakePrismaClient.templates.findUnique.mockResolvedValue(tenplates_fake1);
      FakePrismaClient.templates.findMany.mockResolvedValue(
        template_no_associate,
      );
      FakePrismaClient.templates.delete.mockResolvedValue(tenplates_fake1);

      const return_template_removed = await templatesServices.remove(
        FakePrismaClient,
        removeTemplatesBatchDto_fake,
      );
      expect(return_template_removed.templates.length).toBe(1);
      expect(return_template_removed.templates_associated_devices.length).toBe(
        0,
      );
      expect(return_template_removed.templates_not_found.length).toBe(0);
    });

    it('should return the array templates_associated_devices.', async () => {
      const removeTemplatesBatchDto_fake: RemoveTemplatesBatchDto = {
        templates: [2],
      };

      const { KafkaProducerMock } = KafkaMock.new();
      const FakePrismaClient = PrismaClientMock.new();
      const templatesRepository = new TemplatesRepository(LoggerMock.new());
      const templatesServices = new TemplatesServices(
        LoggerMock.new(),
        templatesRepository,
        KafkaProducerMock,
      );
      FakePrismaClient.templates.findUnique.mockResolvedValue(tenplates_fake1);
      FakePrismaClient.templates.findMany.mockResolvedValue(
        template_with_associate,
      );
      FakePrismaClient.templates.delete.mockResolvedValue(tenplates_fake2);

      const return_template_removed = await templatesServices.remove(
        FakePrismaClient,
        removeTemplatesBatchDto_fake,
      );
      expect(return_template_removed.templates.length).toBe(0);
      expect(return_template_removed.templates_associated_devices.length).toBe(
        1,
      );
      expect(return_template_removed.templates_not_found.length).toBe(0);
    });

    it('should return the array templates_not_found.', async () => {
      const removeTemplatesBatchDto_fake: RemoveTemplatesBatchDto = {
        templates: [200, 211],
      };

      const { KafkaProducerMock } = KafkaMock.new();
      const FakePrismaClient = PrismaClientMock.new();
      const templatesRepository = new TemplatesRepository(LoggerMock.new());
      const templatesServices = new TemplatesServices(
        LoggerMock.new(),
        templatesRepository,
        KafkaProducerMock,
      );
      FakePrismaClient.templates.findUnique.mockResolvedValue(null);

      const return_template_removed = await templatesServices.remove(
        FakePrismaClient,
        removeTemplatesBatchDto_fake,
      );
      expect(return_template_removed.templates.length).toBe(0);
      expect(return_template_removed.templates_associated_devices.length).toBe(
        0,
      );
      expect(return_template_removed.templates_not_found.length).toBe(2);
    });

    it('should return exception', async () => {
      const removeTemplatesBatchDto_fake: RemoveTemplatesBatchDto = {
        templates: [1.0],
      };
      const { KafkaProducerMock } = KafkaMock.new();
      const FakePrismaClient = PrismaClientMock.new();
      const templates_repository = new TemplatesRepository(LoggerMock.new());
      FakePrismaClient.templates.findUnique.mockRejectedValue(devices_fake1);

      const templatesServices = new TemplatesServices(
        LoggerMock.new(),
        templates_repository,
        KafkaProducerMock,
      );

      const fn = () => {
        return templatesServices.remove(
          FakePrismaClient,
          removeTemplatesBatchDto_fake,
        );
      };

      expect(fn).rejects.toThrow(Error);
    });
  });
});
