// Here the unit tests will be written.
import { describe } from '@jest/globals';
import { devices, templates } from '@prisma/client';
import { DevicesRepository } from '../../../../src/app/repository/devicesRepository';
import { LoggerMock, PrismaClientMock } from '../../../mocks';

describe('devicesRepository', () => {
  const devices_fake: devices = {
    id: '1',
    label: 'dev1',
    created: new Date(),
    updated: new Date(),
    persistence: null,
  };

  const templates_fake: templates = {
    id: 1,
    label: 'dev1',
    created: new Date(),
    updated: new Date(),
  };

  describe('findNyId', () => {
    const device_repository = new DevicesRepository(LoggerMock.new());

    it('should findById return null object device.', async () => {
      const FakePrismaClient = PrismaClientMock.new();
      FakePrismaClient.devices.findUnique.mockResolvedValue(null);
      expect(await FakePrismaClient.devices.findUnique).not.toBeCalled();
    });

    it('should findById return one object device.', async () => {
      const FakePrismaClient = PrismaClientMock.new();
      FakePrismaClient.devices.findUnique.mockResolvedValue(devices_fake);
      const device_findById = await device_repository.findById(
        FakePrismaClient,
        '1',
      );
      expect(device_findById).toEqual(devices_fake);
    });

    it('should findById return exception', async () => {
      const FakePrismaClient = PrismaClientMock.new();
      const fn = () => {
        return device_repository.findById(FakePrismaClient, '');
      };

      expect(fn).rejects.toThrow(Error);
    });
  });

  describe('remove', () => {
    const device_repository = new DevicesRepository(LoggerMock.new());
    it('should remove return null object device. ', async () => {
      const FakePrismaClient = PrismaClientMock.new();
      FakePrismaClient.devices.delete.mockRejectedValue({});
      expect(FakePrismaClient.devices.delete).not.toBeCalled();
    });

    it('should remove one object device return object deleted', async () => {
      const FakePrismaClient = PrismaClientMock.new();
      FakePrismaClient.devices.delete.mockResolvedValue(devices_fake);
      const device_removed = await device_repository.remove(
        FakePrismaClient,
        '1',
      );
      expect(device_removed).toEqual(devices_fake);
    });

    it('should remove return exception', async () => {
      const FakePrismaClient = PrismaClientMock.new();
      FakePrismaClient.$disconnect();
      const fn = () => {
        return device_repository.remove(FakePrismaClient, '-1');
      };

      expect(fn).rejects.toThrow(Error);
    });
  });
});
