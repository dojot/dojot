import { Logger } from '@dojot/microservice-sdk';
import { devices, PrismaClient } from '@prisma/client';
import { number } from 'joi';
import { result } from 'lodash';
import { PrismaUtils } from 'src/utils/Prisma.utils';
import { EventKafka, KafkaProducer } from '../../kafka/kafka-producer';
import { CreateDevicesBatchDto } from '../dto/create-devices-batch.dto';
import { RemoveDevicesBatchDto } from '../dto/remove-devices-batch.dto';
import { AttrsRepository } from '../repository/attrsRepository';
import { DevicesRepository } from '../repository/devicesRepository';
import { TemplatesRepository } from '../repository/templatesRepository';

export class DevicesServices {
  constructor(
    private logger: Logger,
    private devicesRepository: DevicesRepository,
    private kafkaproducer: KafkaProducer,
    private prismaUtils: PrismaUtils,
    private templatesRepository: TemplatesRepository,
    private attrsRepository: AttrsRepository,
  ) {
    this.logger.info('Create Constructor DevicesServices', {});
  }
  /**
   * Function remove devices in batch
   * @param connection
   * @param dto
   * @param tenant_id
   * @returns Array of devices removed
   */
  async remove(
    connection: PrismaClient,
    dto: RemoveDevicesBatchDto,
    tenant_id: string,
  ): Promise<any> {
    try {
      let devices_result_batch: Array<any> = [];

      const remove_devices_all_promisses = dto.devices.map(
        async (device_id) => {
          /**
           * Assert device exists
           */
          let device_to_removed = await this.devicesRepository.findById(
            connection,
            device_id.toString(),
          );
          this.logger.debug('Device to Deleted in database', {
            device_to_removed,
          });

          if (device_to_removed) {
            /**
             *  disassociated devices with template.
             */
            await this.devicesRepository.remove_associate_templates(
              connection,
              device_id.toString(),
            );
            /**
             * Remove device of repository data.
             */
            const devices_removed = await this.devicesRepository.remove(
              connection,
              device_id.toString(),
            );
            /**
             * Create Event in Kafka of remove device and publish of message in topic.
             */
            await this.kafkaproducer.send(
              EventKafka.REMOVE,
              tenant_id,
              JSON.stringify({ id: device_id.toString() }),
            );
            this.logger.debug('Object Database Devices Removed', {
              devices_removed,
            });
            /**
             * Add element device in the Array to return.
             */
            devices_result_batch.push({
              id: device_to_removed.id,
              label: device_to_removed.label,
            });
            this.logger.debug('Object Database Devices add Array outputs', {
              devices_result_batch,
            });
          } else {
            /**
             * Add element device NOT_FOUND in repository in the Array to return.
             */
            devices_result_batch.push({
              id: device_id.toString(),
              message: 'Device not found.',
              type: 'NOT_FOUND',
            });
          }
        },
      );
      await Promise.all(remove_devices_all_promisses);
      return { devices: devices_result_batch };
    } catch (error) {
      this.logger.debug('Error', { error });
    }
  }

  async create(
    connection: PrismaClient,
    dto: CreateDevicesBatchDto,
    tenant_id: string,
  ): Promise<any> {
    try {
      let devices_create_result_batch: Array<any> = [];
      let devices_not_create_result_batch: Array<any> = [];
      let templates_not_found: Array<any> = [];
      let attrs_not_found: Array<any> = [];
      let start_sufix = dto.start_sufix;

      for (let index = 0; index < dto.quantity; index++) {
        let name_prefix_device = dto.name_prefix + '-' + start_sufix;
        let device_id_generated = this.prismaUtils.getRandomicHexIdDevices();

        try {
          /**
           * Assert exist object Devices with parameter label = name_prefix_device.
           */
          let assert_devices_exists =
            await this.devicesRepository.assert_devices_exists(
              connection,
              name_prefix_device,
            );
          if (assert_devices_exists) {
            devices_not_create_result_batch.push({
              id: assert_devices_exists?.id,
              label: assert_devices_exists?.label,
            });
          } else {
          }
          /**
           * Create Device in repository.
           */
          let createdDevices = await this.devicesRepository.create(
            connection,
            device_id_generated,
            name_prefix_device,
          );
          this.logger.debug('Devices created in repository.', {
            createdDevices,
          });
          if (createdDevices) {
            /**
             * Add element device in the Array to return.
             */
            devices_create_result_batch.push({
              id: createdDevices?.id,
              label: createdDevices?.label,
            });
            this.logger.debug('Devices created add Array.', {
              devices_create_result_batch,
            });

            /**
             * Function load array of templates, and insert associated devices with templates.
             */
            dto.templates.forEach(async (template_id) => {
              /**
               * Assert template exists
               */
              let assert_template_exists =
                await this.templatesRepository.findById(
                  connection,
                  template_id,
                );
              this.logger.debug('Assert Templates Exists', {
                assert_template_exists,
              });

              if (assert_template_exists) {
                let createdAssociatedDevicesTenplates =
                  await this.devicesRepository.create_associated_devices_templates(
                    connection,
                    device_id_generated,
                    template_id,
                  );
                this.logger.debug(
                  'Devices created Associated Devices in Tenplates.',
                  {
                    createdAssociatedDevicesTenplates,
                  },
                );
              } else {
                templates_not_found.push({
                  id: template_id,
                });
                this.logger.debug('Templates Not Found.', {
                  devices_create_result_batch,
                });
              }
            });
          } else {
          }
        } catch (e) {
          this.logger.error('ERROR - Devices created in repository.', {
            e,
          });
        }
        start_sufix++;
      }

      return {
        devices_created: devices_create_result_batch,
        devices_not_created: devices_not_create_result_batch,
        templates_not_found: templates_not_found,
        attrs_not_found: attrs_not_found,
      };
    } catch (error) {
      this.logger.debug('Error', { error });
    }
  }
}
