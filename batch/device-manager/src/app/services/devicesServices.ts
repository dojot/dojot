import { Logger } from '@dojot/microservice-sdk';
import { PrismaClient } from '@prisma/client';
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
      let attrs_not_found: Array<any> = [];
      let start_sufix = dto.start_sufix;

      await this.validate_repeated_attrs(connection, dto);

      const array_asserts_templates =
        await this.assert_all_templates_valid_exits(connection, dto);

      for (let index = 0; index < dto.quantity; index++) {
        let name_prefix_device = dto.name_prefix + '-' + start_sufix;
        let device_id_generated = this.prismaUtils.getRandomicHexIdDevices();

        if (array_asserts_templates.templates_found.length > 0) {
          /**
           * Assert exist object Device with parameter label = name_prefix_device.
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
              array_asserts_templates.templates_found.map(
                async (id: number) => {
                  let createdAssociatedDevicesTenplates =
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
        templates_not_found: array_asserts_templates.templates_not_found,
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
    const promisse_templates = dto.templates.map(async (template_id) => {
      /**
       * Assert template exists
       */
      let assert_template_exists = await this.templatesRepository.findById(
        connection,
        template_id,
      );

      if (assert_template_exists) {
        templates_found.push(template_id);
      } else {
        templates_not_found.push({
          id: template_id,
          type: 'NOT_FOUND',
          message: 'Template Not Found',
        });
      }
    });
    await Promise.all(promisse_templates);
    return { templates_found, templates_not_found };
  }

  /**
   * Method check exists label repeated in attrs array.
   * @param connection
   * @param dto
   * @return thow excpetion with message.
   */
  async validate_repeated_attrs(
    connection: PrismaClient,
    dto: CreateDevicesBatchDto,
  ): Promise<any> {
    const BreakError = {};
    try {
      if (dto.attrs.length != 0) {
        const promisse_attrs = dto.attrs.map((attrs_result_1) => {
          dto.attrs.map((attrs_result_2) => {
            if (attrs_result_1.id !== attrs_result_2.id) {
              if (attrs_result_1.label === attrs_result_2.label) {
                throw new Error('A device can not have repeated attributes.');
              }
            }
          });
        });
        await Promise.all(promisse_attrs);
      }
    } catch (error) {
      if (error !== BreakError) throw error;
    }
  }
}
