import { Logger, WebUtils } from '@dojot/microservice-sdk';
import { AppConfig, KafkaPayload } from 'src/types';
import { KafkaUtils } from 'src/utils/kafka.utils';
import { PrismaUtils } from 'src/utils/Prisma.utils';

export class TenantManager {
  public tenants: WebUtils.TenantInfo[];

  constructor(
    private logger: Logger,
    private config: AppConfig,
    private dojotHttpClient: WebUtils.DojotHttpClient,
    private prismaUtils: PrismaUtils,
    private kafkaUtils: KafkaUtils,
  ) {
    this.tenants = [];
  }

  private updateTenantSchema(tenant: string) {
    this.prismaUtils.getDatabaseUrl(tenant, this.logger, this.config);
  }

  async update() {
    try {
      this.logger.info('Updating list of tenants', {});

      const response = await this.dojotHttpClient.request({
        method: 'GET',
        url: this.config.keycloak['tenants.url'],
        timeout: 15000,
      });

      this.logger.info('Running migrations for all tenants', {});

      this.tenants = response.data.tenants;
      this.tenants.forEach((tenant) => this.updateTenantSchema(tenant.id));

      this.logger.info('Migrations ran successfully', {});
    } catch (e: unknown) {
      const error = e as Error;
      this.logger.info(
        `Failed to update list of tenants: ${error.stack || error}`,
        {},
      );
    }
  }

  create(payload: KafkaPayload) {
    try {
      const value = this.kafkaUtils.getValue(payload);
      this.logger.info(`${value.type} bucket for ${value.tenant} tenant`, {});
      //this.updateTenantSchema(value.tenant)
    } catch (e: unknown) {
      const error = e as Error;
      this.logger.info(`Tenant creation failed: ${error.stack || error}`, {});
    }
  }
}
