// Here the unit tests will be written.
import { describe } from '@jest/globals';
import { templates } from '@prisma/client';

import { LoggerMock, PrismaClientMock } from '../../../mocks';
import { TemplatesRepository } from '../../../../src/app/repository/';

describe('TemplatesRepository', () => {
  const templates_fake1: templates = {
    id: 1,
    label: 'modelo_1',
    created: new Date(),
    updated: new Date(),
  };

  describe('findById', () => {
    const templates_repository = new TemplatesRepository(LoggerMock.new());

    it('should findById return null object template.', async () => {
      const FakePrismaClient = PrismaClientMock.new();
      FakePrismaClient.templates.findUnique.mockResolvedValue(null);

      const template_findById = await templates_repository.findById(
        FakePrismaClient,
        1,
      );

      expect(template_findById).toBeNull();
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
      expect.assertions(1);
      const FakePrismaClient = PrismaClientMock.new();
      FakePrismaClient.templates.findUnique.mockRejectedValueOnce('error');

      try {
        await templates_repository.findById(FakePrismaClient, -1);
      } catch (err) {
        expect(err).toBeDefined();
      }
    });
  });

  describe('findByTemplateAssociatesDevicesOrNot', () => {
    it('should return null object templates. ', async () => {
      const FakePrismaClient = PrismaClientMock.new();
      const templates_repository = new TemplatesRepository(LoggerMock.new());
      FakePrismaClient.templates.findMany.mockResolvedValue([]);
      const result =
        await templates_repository.findByTemplateAssociatesDevicesOrNot(
          FakePrismaClient,
          0,
        );
      expect(result.length).toBe(0);
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

    it('should findByTemplateAssociatesDevicesOrNot return exception', async () => {
      expect.assertions(1);
      const templates_repository = new TemplatesRepository(LoggerMock.new());
      const FakePrismaClient = PrismaClientMock.new();
      FakePrismaClient.templates.findMany.mockRejectedValue('Error');

      try {
        await templates_repository.findByTemplateAssociatesDevicesOrNot(
          FakePrismaClient,
          -1,
        );
      } catch (error) {
        expect(error).toBeDefined();
      }
    });
  });

  describe('remove', () => {
    const templates_repository = new TemplatesRepository(LoggerMock.new());

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
      expect.assertions(1);
      const FakePrismaClient = PrismaClientMock.new();
      const templates_repository = new TemplatesRepository(LoggerMock.new());
      FakePrismaClient.templates.delete.mockRejectedValue({});
      try {
        await templates_repository.remove(FakePrismaClient, 1);
      } catch (err) {
        expect(err).toBeDefined();
      }
    });
  });

  describe('findByIdWithAttrs', () => {
    const templates_repository = new TemplatesRepository(LoggerMock.new());
    it('should findByIdWithAttrs return null object templates. ', async () => {
      const FakePrismaClient = PrismaClientMock.new();
      FakePrismaClient.templates.findUnique.mockResolvedValueOnce(null);

      const result = await templates_repository.findByIdWithAttrs(
        FakePrismaClient,
        1,
      );

      expect(result).toBeNull();
    });
  });
});
