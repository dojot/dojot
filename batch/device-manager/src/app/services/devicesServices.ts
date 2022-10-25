import { Logger } from '@dojot/microservice-sdk';
import { PrismaClient } from '@prisma/client';
import { EventKafka, KafkaProducer } from '../../kafka/kafka-producer';
import { Devices } from 'src/types/Device.types';
import { RemoveDevicesBatchDto } from '../dto/remove-devices-batch.dto';
import { DevicesRepository } from '../repository/devicesRepository';

export class DevicesServices {
  constructor(
    private logger: Logger,
    private devicesRepository: DevicesRepository,
    private kafkaproducer: KafkaProducer,
  ) {
    this.logger.info('Create Constructor DevicesServices', {});
  }

  async remove(
    connection: PrismaClient,
    dto: RemoveDevicesBatchDto,
    tenant_id: string,
  ): Promise<any> {
    try {
      let devices_removed_batch: Array<Devices> = [];
      const remove_devices_all_promisses = dto.devices.map(
        async (device_id) => {
          let device_to_removed = await this.devicesRepository.findById(
            connection,
            device_id.toString(),
          );
          this.logger.debug('Device to Deleted in database', {
            device_to_removed,
          });
          if (device_to_removed) {
            await this.devicesRepository.remove_associate_templates(
              connection,
              device_id.toString(),
            );
            const devices_removed = await this.devicesRepository.remove(
              connection,
              device_id.toString(),
            );
            await this.kafkaproducer.send(
              EventKafka.REMOVE,
              tenant_id,
              JSON.stringify({ id: device_id.toString() }),
            );
            this.logger.debug('Object Database Devices Removed', {
              devices_removed,
            });
            devices_removed_batch.push({
              id: device_to_removed.id,
              label: device_to_removed.label,
            });
            this.logger.debug('Object Database Devices add Array outputs', {
              devices_removed_batch,
            });
          }
        },
      );
      await Promise.all(remove_devices_all_promisses);
      return devices_removed_batch;
    } catch (error) {
      this.logger.debug('Error', { error });
    }
  }
}
