import { Logger } from '@dojot/microservice-sdk';
import { PrismaUtils } from 'src/utils/Prisma.utils';
import { PrismaClient } from '@prisma/client';

import { EventKafka, KafkaProducer } from '../../kafka/kafka-producer';
import { CreateDevicesBatchDto } from '../dto/create-devices-batch.dto';
import { RemoveDevicesBatchDto } from '../dto/remove-devices-batch.dto';
import { DevicesRepository } from '../repository/devicesRepository';
import { TemplatesRepository } from '../repository/templatesRepository';
import { KafkaEventData } from '../../types/Kafka.types';

type DeviceResultBatch = {
  id: string;
  label: string;
};

type DeviceNotFoundBatch = {
  id: string;
  message: string;
  type: string;
};

export class DevicesServices {
  constructor(
    private logger: Logger,
    private devicesRepository: DevicesRepository,
    private kafkaproducer: KafkaProducer,
    private prismaUtils: PrismaUtils,
    private templatesRepository: TemplatesRepository,
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
      const devices_result_batch: Array<DeviceResultBatch> = [];
      const devices_not_found_batch: Array<DeviceNotFoundBatch> = [];
      const remove_devices_all_promisses = dto.devices.map(
        async (device_id) => {
          /**
           * Assert device exists
           */
          const assert_device_exists =
            await this.devicesRepository.findByIdWithTemplatesAttrs(
              connection,
              device_id.toString(),
            );
          this.logger.debug('Device to Deleted in database', {
            assert_device_exists,
          });

          if (assert_device_exists != null) {
            const templates_associated_with_device: Array<number> = [];
            const attrs_associated_with_template_and_device: Array<any> = [];

            assert_device_exists.device_template.forEach(
              async (element: any) => {
                templates_associated_with_device.push(element.templates.id);
                attrs_associated_with_template_and_device.push({
                  [element.templates.id.toString()]: element.templates.attrs,
                });
              },
            );

            /**
             *  disassociated devices with template.
             */
            await this.devicesRepository.remove_associate_templates(
              connection,
              device_id.toString(),
            );

            /**
             *  disassociated devices with overrides.
             */
            await this.devicesRepository.remove_associate_overrides(
              connection,
              device_id.toString(),
            );

            /**
             *  disassociated devices with pre_shared_keys.
             */
            await this.devicesRepository.remove_associate_pre_shared_keys(
              connection,
              device_id.toString(),
            );
            /**
             * Remove device of repository data.
             */
            const removed_device = await this.devicesRepository.remove(
              connection,
              device_id.toString(),
            );

            /**
             * Create Event in Kafka of remove device and publish of message in topic.
             */
            if (removed_device) {
              const data = this.create_body_of_field_data_published_kafka(
                assert_device_exists,
                templates_associated_with_device,
                attrs_associated_with_template_and_device,
              );
              await this.kafkaproducer.send(EventKafka.REMOVE, tenant_id, data);
              this.logger.debug('Send Message Kafka - REMOVE Device.', {
                removed_device,
              });
              /**
               * Add element device in the Array to return.
               */
              devices_result_batch.push({
                id: removed_device.id,
                label: removed_device.label,
              });
              this.logger.debug(
                'Object Database Devices add Array outputs devices.',
                {
                  devices_result_batch,
                },
              );
            }
          } else {
            /**
             * Add element device NOT_FOUND in repository in the Array to return.
             */
            devices_not_found_batch.push({
              id: device_id.toString(),
              message: 'Device not found.',
              type: 'NOT_FOUND',
            });

            this.logger.debug('Devices add Array outputs not found.', {
              devices_not_found_batch,
            });
          }
        },
      );
      await Promise.all(remove_devices_all_promisses);

      return {
        devices: devices_result_batch,
        devices_not_found: devices_not_found_batch,
      };
    } catch (error) {
      this.logger.debug('Error', { error });
    }
  }

  /**
   * Method Create Devices in batch
   * @param connection
   * @param dto
   * @param tenant_id
   * @returns Return Array creted Devices and Array not created and Array Not Found Templates.
   */
  async create(
    connection: PrismaClient,
    dto: CreateDevicesBatchDto,
    tenant_id: string,
  ): Promise<any> {
    try {
      const devices_create_result_batch: Array<any> = [];
      const devices_not_create_result_batch: Array<any> = [];
      const attrs_not_found: Array<any> = [];
      let start_sufix = dto.start_sufix;

      const array_asserts_templates_and_attrs_not_found_templates =
        await this.assert_all_templates_valid_exits(connection, dto);
      for (let index = 0; index < dto.quantity; index++) {
        const name_prefix_device = dto.name_prefix + '-' + start_sufix;
        const device_id_generated = this.prismaUtils.getRandomicHexIdDevices();

        if (
          array_asserts_templates_and_attrs_not_found_templates.templates_found
            .length > 0
        ) {
          /**
           * Assert exist object Device with parameter label = name_prefix_device.
           */
          const assert_devices_exists =
            await this.devicesRepository.assert_devices_exists(
              connection,
              name_prefix_device,
            );

          if (assert_devices_exists) {
            devices_not_create_result_batch.push({
              id: assert_devices_exists.id,
              label: assert_devices_exists.label,
            });
          } else {
            /**
             * Create Device in repository.
             */
            const createdDevices = await this.devicesRepository.create(
              connection,
              device_id_generated,
              name_prefix_device,
            );
            this.logger.debug('Devices created in repository.', {
              createdDevices,
            });
            if (createdDevices) {
              const data = this.create_body_of_field_data_published_kafka(
                createdDevices,
                array_asserts_templates_and_attrs_not_found_templates.templates_found,
                array_asserts_templates_and_attrs_not_found_templates.attrs_found,
              );

              await this.kafkaproducer.send(EventKafka.CREATE, tenant_id, data);
              this.logger.debug('Send Message Kafka - CREATE Device.', {
                createdDevices,
              });

              /**
               * Add element device in the Array to return.
               */
              devices_create_result_batch.push({
                id: createdDevices.id,
                label: createdDevices.label,
              });
              this.logger.debug('Devices created add Array.', {
                devices_create_result_batch,
              });

              /**
               * Function load array of templates, and insert associated devices with templates.
               */
              array_asserts_templates_and_attrs_not_found_templates.templates_found.map(
                async (id: number) => {
                  const createdAssociatedDevicesTenplates =
                    await this.devicesRepository.create_associated_devices_templates(
                      connection,
                      device_id_generated,
                      id,
                    );
                  this.logger.debug(
                    'Devices created Associated Devices in Tenplates.',
                    {
                      createdAssociatedDevicesTenplates,
                    },
                  );
                },
              );
            }
          }
        }
        start_sufix++;
      }
      return {
        devices_created: devices_create_result_batch,
        devices_not_created: devices_not_create_result_batch,
        templates_not_found:
          array_asserts_templates_and_attrs_not_found_templates.templates_not_found,
        attrs_not_found: attrs_not_found,
      };
    } catch (e) {
      const error = e as Error;
      this.logger.debug('DevicesRepository - create_devices in batch ', {
        error: error.message,
      });
      throw new Error(error.message);
    }
  }

  /**
   * Method check exists all id template in the array with thr repository.
   * @param connection
   * @param dtothis
   * @return Return two array with id found in repository and id not found repository
   */
  async assert_all_templates_valid_exits(
    connection: PrismaClient,
    dto: CreateDevicesBatchDto,
  ): Promise<any> {
    const templates_not_found: Array<any> = [];
    const templates_found: Array<any> = [];
    const attrs_found: Array<any> = [];
    const promisse_templates = dto.templates.map(async (template_id) => {
      /**
       * Assert template and attrs exists
       */
      const assert_template_exists =
        await this.templatesRepository.findByIdWithAttrs(
          connection,
          template_id,
        );

      if (assert_template_exists) {
        templates_found.push(template_id);
        attrs_found.push({ [template_id]: assert_template_exists.attrs });
      } else {
        templates_not_found.push({
          id: template_id,
          type: 'NOT_FOUND',
          message: 'Template Not Found',
        });
      }
    });
    await Promise.all(promisse_templates);
    return { templates_found, templates_not_found, attrs_found };
  }

  /**
   * Method to mounted message data of field in message send kafka.
   * @param device_event
   * @param templates_event
   * @param attrs_event
   * @returns Retuen object data to send.
   */
  create_body_of_field_data_published_kafka(
    device_event: any,
    templates_event: Array<number>,
    attrs_event: Array<any>,
  ) {
    const EventData: KafkaEventData = {
      id: device_event.id,
      label: device_event.label,
      created: device_event.created,
      templates: templates_event,
      attrs: attrs_event,
    };
    return Object.assign(EventData);
  }
}
