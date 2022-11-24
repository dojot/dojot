// Here the unit tests will be written.
import { describe } from '@jest/globals';
import { devices, device_template, templates } from '@prisma/client';
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
    label: 'modelo1',
    created: new Date(),
    updated: new Date(),
  };

  const device_template_fake: device_template = {
    device_id: '100',
    template_id: 100,
  };

  describe('findById', () => {
    const device_repository = new DevicesRepository(LoggerMock.new());

    it('should findById return null object device.', async () => {
      const FakePrismaClient = PrismaClientMock.new();
      FakePrismaClient.devices.findUnique.mockResolvedValue(null);
      expect(await FakePrismaClient.devices.findUnique).not.toBeCalled();
    });

    it('should findById return one object device.', async () => {
      const FakePrismaClient = PrismaClientMock.new();
      FakePrismaClient.devices.findUnique.mockResolvedValue(devices_fake);
      const device_found = await device_repository.findByIdWithTemplatesAttrs(
        FakePrismaClient,
        '1',
      );
      expect(device_found).toEqual(devices_fake);
    });

    it('should findById return exception', async () => {
      const FakePrismaClient = PrismaClientMock.new();
      const device_repository = new DevicesRepository(LoggerMock.new());
      FakePrismaClient.devices.findMany.mockRejectedValue([templates_fake]);
      const fn = () => {
        return device_repository.findByIdWithTemplatesAttrs(
          FakePrismaClient,
          '1',
        );
      };

      expect(fn).rejects.toThrow(Error);
    });
  });

  describe('remove', () => {
    const device_repository = new DevicesRepository(LoggerMock.new());
    it('should remove return null object device. ', async () => {
      const FakePrismaClient = PrismaClientMock.new();
      FakePrismaClient.devices.delete.mockRejectedValue({});
      await device_repository.remove(FakePrismaClient, '1');
      expect(FakePrismaClient.devices.delete).toBeCalled();
    });

    it('should remove return exception', async () => {
      const FakePrismaClient = PrismaClientMock.new();
      const device_repository = new DevicesRepository(LoggerMock.new());
      FakePrismaClient.devices.delete.mockRejectedValue({ templates_fake });
      const fn = () => {
        return device_repository.remove(FakePrismaClient, '1');
      };

      expect(fn).rejects.toThrow(Error);
    });

    it('should remove_associate_templates return exception', async () => {
      const FakePrismaClient = {} as any;
      const device_repository = new DevicesRepository(LoggerMock.new());
      const fn = () => {
        return device_repository.remove_associate_templates(
          FakePrismaClient,
          '',
        );
      };

      expect(fn).rejects.toThrow(Error);
    });
  });

  describe('assert_devices_exists', () => {
    const device_repository = new DevicesRepository(LoggerMock.new());

    it('should assert device exists return one object device.', async () => {
      const FakePrismaClient = PrismaClientMock.new();
      FakePrismaClient.devices.findUnique.mockResolvedValue(devices_fake);
      const device_repository = new DevicesRepository(LoggerMock.new());
      const assert_devices_exists =
        await device_repository.assert_devices_exists(FakePrismaClient, 'dev1');
      expect(FakePrismaClient.devices.findUnique).toBeCalled();
      expect(assert_devices_exists).toEqual(devices_fake);
    });

    it('should assert_devices_exists return exception', async () => {
      const FakePrismaClient = {} as any;
      const device_repository = new DevicesRepository(LoggerMock.new());
      const fn = () => {
        return device_repository.assert_devices_exists(FakePrismaClient, '');
      };

      expect(fn).rejects.toThrow(Error);
    });
  });

  describe('create', () => {
    const device_repository = new DevicesRepository(LoggerMock.new());

    it('should call create device in batch .', async () => {
      const FakePrismaClient = PrismaClientMock.new();
      const device_repository = new DevicesRepository(LoggerMock.new());
      FakePrismaClient.devices.create.mockResolvedValue(devices_fake);
      const device_created = await device_repository.create(
        FakePrismaClient,
        '1',
        'teste',
      );
      expect(FakePrismaClient.devices.create).toBeCalled();
    });

    it('should call create device and create associated in batch.', async () => {
      const FakePrismaClient = PrismaClientMock.new();
      const device_repository = new DevicesRepository(LoggerMock.new());
      FakePrismaClient.devices.create.mockResolvedValue(devices_fake);
      FakePrismaClient.device_template.create.mockResolvedValue(
        device_template_fake,
      );
      const device_created = await device_repository.create(
        FakePrismaClient,
        '1',
        'teste',
      );
      await device_repository.create_associated_devices_templates(
        FakePrismaClient,
        '1',
        1,
      );
      expect(FakePrismaClient.devices.create).toBeCalled();
      expect(FakePrismaClient.device_template.create).toBeCalled();
    });

    it('should remove_associate_overrides return exception', async () => {
      const FakePrismaClient = {} as any;
      const device_repository = new DevicesRepository(LoggerMock.new());
      const fn = () => {
        return device_repository.remove_associate_overrides(
          FakePrismaClient,
          '',
        );
      };

      expect(fn).rejects.toThrow(Error);
    });

    it('should remove_associate_pre_shared_keys return exception', async () => {
      const FakePrismaClient = {} as any;
      const device_repository = new DevicesRepository(LoggerMock.new());
      const fn = () => {
        return device_repository.remove_associate_pre_shared_keys(
          FakePrismaClient,
          '',
        );
      };

      expect(fn).rejects.toThrow(Error);
    });

    it('should create return exception', async () => {
      const FakePrismaClient = {} as any;
      const device_repository = new DevicesRepository(LoggerMock.new());
      const fn = () => {
        return device_repository.create(FakePrismaClient, '', '');
      };

      expect(fn).rejects.toThrow(Error);
    });

    it('should return exception in create devices and templates associoted ', async () => {
      const FakePrismaClient = {} as any;
      const device_repository = new DevicesRepository(LoggerMock.new());
      const fn = () => {
        return device_repository.create_associated_devices_templates(
          FakePrismaClient,
          '',
          -1,
        );
      };

      expect(fn).rejects.toThrow(Error);
    });
  });
});
