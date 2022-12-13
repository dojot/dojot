// Here the unit tests will be written.
import { describe } from '@jest/globals';
import {
  devices,
  device_template,
  PrismaClient,
  templates,
} from '@prisma/client';

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

      const result = await device_repository.findByIdWithTemplatesAttrs(
        FakePrismaClient,
        'id',
      );

      expect(result).toBeNull();
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
      expect.assertions(1);
      const FakePrismaClient = PrismaClientMock.new();
      const device_repository = new DevicesRepository(LoggerMock.new());
      FakePrismaClient.devices.findUnique.mockRejectedValue([templates_fake]);

      try {
        await device_repository.findByIdWithTemplatesAttrs(
          FakePrismaClient,
          '1',
        );
      } catch (err) {
        expect(err).toBeDefined();
      }
    });
  });

  describe('remove', () => {
    const device_repository = new DevicesRepository(LoggerMock.new());
    it('should remove return null object device. ', async () => {
      const FakePrismaClient = PrismaClientMock.new();
      const shouldResult = {
        id: 'id',
        label: 'label',
        created: new Date(),
        updated: new Date(),
        persistence: null,
      };

      FakePrismaClient.devices.delete.mockResolvedValueOnce(shouldResult);

      const result = await device_repository.remove(FakePrismaClient, '1');

      expect(result).toEqual(shouldResult);
    });

    it('should remove return exception', async () => {
      expect.assertions(1);
      const FakePrismaClient = PrismaClientMock.new();
      const device_repository = new DevicesRepository(LoggerMock.new());
      FakePrismaClient.devices.delete.mockRejectedValue({ templates_fake });

      try {
        await device_repository.remove(FakePrismaClient, '1');
      } catch (err) {
        expect(err).toBeDefined();
      }
    });

    it('should remove_associate_templates return exception', async () => {
      expect.assertions(1);
      const FakePrismaClient = {} as any;
      const device_repository = new DevicesRepository(LoggerMock.new());

      try {
        await device_repository.remove_associate_templates(
          FakePrismaClient,
          '',
        );
      } catch (err) {
        expect(err).toBeDefined();
      }
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
      expect.assertions(1);
      const FakePrismaClient = {} as any;
      const device_repository = new DevicesRepository(LoggerMock.new());
      try {
        await device_repository.assert_devices_exists(FakePrismaClient, '');
      } catch (err) {
        expect(err).toBeDefined();
      }
    });
  });

  describe('create', () => {
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
      await device_repository.create_associated_devices_templates(
        FakePrismaClient,
        '1',
        1,
      );

      expect(FakePrismaClient.device_template.create).toBeCalled();
    });

    it('should remove_associate_overrides return exception', async () => {
      expect.assertions(1);
      const FakePrismaClient = {} as PrismaClient;
      const device_repository = new DevicesRepository(LoggerMock.new());

      try {
        await device_repository.remove_associate_overrides(
          FakePrismaClient,
          '',
        );
      } catch (err) {
        expect(err).toBeDefined();
      }
    });

    it('should remove_associate_pre_shared_keys return exception', async () => {
      expect.assertions(1);
      const FakePrismaClient = {} as PrismaClient;
      const device_repository = new DevicesRepository(LoggerMock.new());
      try {
        await device_repository.remove_associate_pre_shared_keys(
          FakePrismaClient,
          '',
        );
      } catch (err) {
        expect(err).toBeDefined();
      }
    });

    it('should create return exception', async () => {
      expect.assertions(1);
      const FakePrismaClient = {} as PrismaClient;
      const device_repository = new DevicesRepository(LoggerMock.new());
      try {
        await device_repository.create(FakePrismaClient, '', '');
      } catch (err) {
        expect(err).toBeDefined();
      }
    });

    it('should return exception in create devices and templates associoted ', async () => {
      expect.assertions(1);
      const FakePrismaClient = {} as PrismaClient;
      const device_repository = new DevicesRepository(LoggerMock.new());
      try {
        await device_repository.create_associated_devices_templates(
          FakePrismaClient,
          '',
          -1,
        );
      } catch (err) {
        expect(err).toBeDefined();
      }
    });
  });
});
