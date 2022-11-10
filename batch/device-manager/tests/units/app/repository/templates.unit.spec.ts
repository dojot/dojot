// Here the unit tests will be written.
import { describe } from '@jest/globals';
import { devices, device_template, templates } from '@prisma/client';
import { LoggerMock, PrismaClientMock } from '../../../mocks';
import { TemplatesRepository } from '../../../../src/app/repository/';

describe('TemplatesRepository', () => {
  const devices_fake: devices = {
    id: '1',
    label: 'sensor_1',
    created: new Date(),
    updated: new Date(),
    persistence: null,
  };

  const templates_fake1: templates = {
    id: 1,
    label: 'modelo_1',
    created: new Date(),
    updated: new Date(),
  };

  const templates_fake2: device_template = {
    device_id: '',
    template_id: 0,
  };

  const devices_templates_with_associate_fake = [
    {
      templates: {
        id: 1,
        label: 'modelo1',
        device_template: [
          {
            devices: {
              id: '2',
              label: 'dev2',
            },
          },
          {
            devices: {
              id: '3',
              label: 'dev3',
            },
          },
        ],
      },
    },
  ];

  const devices_templates_not_with_associate_fake = {
    templates: {
      id: 1,
      label: 'modelo1',
      created: null,
      updated: null,
      device_template: [],
    },
  };

  describe('findById', () => {
    const templates_repository = new TemplatesRepository(LoggerMock.new());

    it('should findById return null object template.', async () => {
      const FakePrismaClient = PrismaClientMock.new();
      FakePrismaClient.templates.findUnique.mockResolvedValue(null);
      expect(await FakePrismaClient.templates.findUnique).not.toBeCalled();
    });

    it('should findById return one object template.', async () => {
      const FakePrismaClient = PrismaClientMock.new();
      FakePrismaClient.templates.findUnique.mockResolvedValue(templates_fake1);
      const template_findById = await templates_repository.findById(
        FakePrismaClient,
        1,
      );
      expect(template_findById).toEqual(templates_fake1);
    });

    it('should findById return exception', async () => {
      const FakePrismaClient = PrismaClientMock.new();
      const fn = () => {
        return templates_repository.findById(FakePrismaClient, -1);
      };

      expect(fn).rejects.toThrow(Error);
    });
  });

  describe('findByTemplateAssociatesDevicesOrNot', () => {
    it('should return null object templates. ', async () => {
      const FakePrismaClient = PrismaClientMock.new();
      const templates_repository = new TemplatesRepository(LoggerMock.new());
      FakePrismaClient.templates.findMany.mockRejectedValue({});
      await templates_repository.findByTemplateAssociatesDevicesOrNot(
        FakePrismaClient,
        0,
      );
      expect(FakePrismaClient.templates.findMany).toBeCalled();
    });

    it('should return object templates with associate devices.', async () => {
      const FakePrismaClient = PrismaClientMock.new();
      const templates_repository = new TemplatesRepository(LoggerMock.new());
      FakePrismaClient.templates.findMany.mockResolvedValue([]);
      const template_found_with_associate =
        await templates_repository.findByTemplateAssociatesDevicesOrNot(
          FakePrismaClient,
          1,
        );
      expect(template_found_with_associate).toHaveLength(0);
    });

    it('should return object templates not with associate devices. ', async () => {
      const templates_repository = new TemplatesRepository(LoggerMock.new());
      const FakePrismaClient = PrismaClientMock.new();
      FakePrismaClient.templates.findMany.mockResolvedValue([templates_fake1]);
      const template_found_not_with_associate =
        await templates_repository.findByTemplateAssociatesDevicesOrNot(
          FakePrismaClient,
          1,
        );

      expect(template_found_not_with_associate).toEqual([templates_fake1]);
    });
    it('should findById return exception', async () => {
      const templates_repository = new TemplatesRepository(LoggerMock.new());
      const FakePrismaClient = PrismaClientMock.new();
      FakePrismaClient.templates.findMany.mockRejectedValue(templates_fake1);
      const fn = () => {
        return templates_repository.findByTemplateAssociatesDevicesOrNot(
          FakePrismaClient,
          1,
        );
      };

      expect(fn).rejects.toThrow(Error);
    });
  });

  describe('remove', () => {
    const templates_repository = new TemplatesRepository(LoggerMock.new());
    it('should remove return null object templates. ', async () => {
      const FakePrismaClient = PrismaClientMock.new();
      FakePrismaClient.templates.delete.mockRejectedValue({});
      expect(FakePrismaClient.devices.delete).not.toBeCalled();
    });

    it('should remove one object device return object deleted', async () => {
      const FakePrismaClient = PrismaClientMock.new();
      FakePrismaClient.templates.delete.mockResolvedValue(templates_fake1);
      const template_removed = await templates_repository.remove(
        FakePrismaClient,
        1,
      );
      expect(template_removed).toEqual(templates_fake1);
    });

    it('should remove return exception', async () => {
      const FakePrismaClient = PrismaClientMock.new();
      const templates_repository = new TemplatesRepository(LoggerMock.new());
      FakePrismaClient.templates.delete.mockRejectedValue({ devices_fake });
      const fn = () => {
        return templates_repository.remove(FakePrismaClient, 1);
      };

      expect(fn).rejects.toThrow(Error);
    });
  });
});
