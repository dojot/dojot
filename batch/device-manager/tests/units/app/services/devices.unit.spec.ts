// Here the unit tests will be written.
import { describe } from '@jest/globals';
import { devices } from '@prisma/client';
import { RemoveDevicesBatchDto } from 'src/types';
import { DevicesServices } from '../../../../src/app/services/devicesServices';
import { KafkaMock, LoggerMock, PrismaClientMock } from '../../../mocks';
import { DevicesRepository } from '../../../../src/app/repository/devicesRepository';

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
      //const { DeviceRepositoryMock } = AppMock.new();
      const FakePrismaClient = PrismaClientMock.new();
      const devicesRepository = new DevicesRepository(LoggerMock.new());

      const devicesServices = new DevicesServices(
        LoggerMock.new(),
        devicesRepository,
        KafkaProducerMock,
      );
      FakePrismaClient.devices.findUnique.mockResolvedValue(devices_fake);
      const return_devices_removed = await devicesServices.remove(
        FakePrismaClient,
        removeDevicesBatchDto_fake,
        '',
      );
      expect(return_devices_removed).toHaveLength(1);
    });
  });
});
