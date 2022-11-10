// Here the unit tests will be written.
import { describe } from '@jest/globals';
import { attrs, templates } from '@prisma/client';
import { AttrsRepository } from '../../../../src/app/repository/attrsRepository';
import { LoggerMock, PrismaClientMock } from '../../../mocks';

describe('attrsRepository', () => {
  const attrs_fake: attrs = {
    id: 1,
    label: 'teste',
    created: new Date(),
    updated: new Date(),
    type: 'dynamic',
    value_type: 'bool',
    static_value: null,
    template_id: 1,
    parent_id: null,
  };

  const template_fake: templates = {
    id: 1,
    label: 'teste',
    created: new Date(),
    updated: new Date(),
  };

  describe('findById', () => {
    const attrs_repository = new AttrsRepository(LoggerMock.new());

    it('should findById return null object attrs.', async () => {
      const FakePrismaClient = PrismaClientMock.new();
      const attrs_repository = new AttrsRepository(LoggerMock.new());
      FakePrismaClient.attrs.findUnique.mockResolvedValue(null);
      attrs_repository.findById(FakePrismaClient, 1);
      expect(FakePrismaClient.attrs.findUnique).toBeCalled();
    });

    it('should findById return one object attr.', async () => {
      const FakePrismaClient = PrismaClientMock.new();
      FakePrismaClient.attrs.findUnique.mockResolvedValue(attrs_fake);
      const attrs_findById = await attrs_repository.findById(
        FakePrismaClient,
        1,
      );
      expect(attrs_findById).toEqual(attrs_fake);
    });

    it('should findById return exception', async () => {
      const FakePrismaClient = PrismaClientMock.new();
      FakePrismaClient.attrs.findUnique.mockRejectedValue([template_fake]);
      const fn = () => {
        return attrs_repository.findById(FakePrismaClient, 1);
      };

      expect(fn).rejects.toThrow(Error);
    });
  });
});
