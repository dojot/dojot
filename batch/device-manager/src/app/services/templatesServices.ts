import { Logger } from '@dojot/microservice-sdk';
import { PrismaClient } from '@prisma/client';
import { KafkaProducer } from '../../kafka/kafka-producer';
import { RemoveTemplatesBatchDto } from 'src/types';
import { TemplatesRepository } from '../repository';

export class TemplatesServices {
  constructor(
    private logger: Logger,
    private templatesRepository: TemplatesRepository,
    private kafkaproducer: KafkaProducer,
  ) {
    this.logger.info('Create Constructor TemplatesServices', {});
  }

  /**
   * Function remove templates in batch
   * @param connection
   * @param dto
   * @param tenant_id
   * @returns Array of templates removed
   */
  async remove(
    connection: PrismaClient,
    dto: RemoveTemplatesBatchDto,
  ): Promise<any> {
    try {
      let templates_removed_batch: Array<any> = [];
      let templates_not_found_batch: Array<any> = [];
      let templates_associated_devices_batch: Array<any> = [];
      let devices_associated_templatesd_batch: Array<any> = [];
      let aux_ids_device_found_associated: Array<any> = [];

      let template_to_removed;
      const remove_templates_all_promisses = dto.templates.map(
        async (template_id) => {
          /**
           * Assert template exists
           */
          let assert_template_exists = await this.templatesRepository.findById(
            connection,
            template_id,
          );
          this.logger.debug('Assert Template Exists', {
            assert_template_exists,
          });
          if (assert_template_exists) {
            template_to_removed =
              await this.templatesRepository.findByTemplateAssociatesDevicesOrNot(
                connection,
                assert_template_exists.id,
              );
            this.logger.debug('Template to Removed in batch', {
              template_to_removed,
            });
            /**
             * Assert Exist Associated Devices in found.
             */
            if (typeof template_to_removed !== 'undefined') {
              let qt_associated_with_devices =
                template_to_removed[0].device_template.length;

              if (qt_associated_with_devices >= 1) {
                template_to_removed[0].device_template.forEach(
                  (devices_result) => {
                    if (
                      !aux_ids_device_found_associated.includes(
                        devices_result.devices.id,
                      )
                    ) {
                      devices_associated_templatesd_batch.push({
                        id: devices_result.devices.id,
                        label: devices_result.devices.label,
                      });
                      aux_ids_device_found_associated.push(
                        devices_result.devices.id,
                      );
                    }
                  },
                );
                templates_associated_devices_batch.push({
                  id: assert_template_exists.id,
                  label: assert_template_exists.label,
                  type: 'HAS_ASSOCIATED_DEVICES',
                  message: 'The template has associated devices',
                  associated_devices: devices_associated_templatesd_batch,
                });
              } else {
                /**
                 * Remove template found in repository.
                 */
                let template_removed = await this.templatesRepository.remove(
                  connection,
                  template_id,
                );
                this.logger.debug('Template Removed in repository', {
                  template_removed,
                });
                /**
                 * Add element template in the Array to return.
                 */
                templates_removed_batch.push({
                  id: assert_template_exists.id,
                  label: assert_template_exists.label,
                });
              }
            }
          } else {
            templates_not_found_batch.push({
              id: template_id,
              type: 'NOT_FOUND',
              message: 'Template Not Found',
            });
          }
        },
      );
      await Promise.all(remove_templates_all_promisses);
      return {
        templates: templates_removed_batch,
        templates_associated_devices: templates_associated_devices_batch,
        templates_not_found: templates_not_found_batch,
      };
    } catch (error) {
      this.logger.debug('Error', { error });
    }
  }
}