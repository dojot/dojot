import { Logger, WebUtils } from '@dojot/microservice-sdk';

import { AppConfig } from 'src/types';
import { PrismaUtils } from 'src/utils/Prisma.utils';

type CreateTenantParams = {
  tenant: string;
  // eslint-disable-next-line @typescript-eslint/ban-types
  signatureKey: object;
};

export class TenantManager {
  public tenants: WebUtils.TenantInfo[];

  constructor(
    private logger: Logger,
    private config: AppConfig,
    private dojotHttpClient: WebUtils.DojotHttpClient,
    private prismaUtils: PrismaUtils,
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
      this.logger.info('update: updating the schema of all tenants', {});
      this.tenants = response.data.tenants;
      this.tenants.forEach((tenant) => {
        this.logger.info(`update: updating schema for ${tenant.id}`, {});
        this.updateTenantSchema(tenant.id);
      });

      this.logger.info(
        'update: all schemas have been updated successfully',
        {},
      );
    } catch (e) {
      this.logger.error('update: failed to update list of tenants', e as never);
    }
  }

  async create({ tenant, signatureKey }: CreateTenantParams) {
    try {
      this.logger.info(`create: creating tenant ${tenant}`, {});
      this.updateTenantSchema(tenant);
      const newTenant = { id: tenant, signatureKey, sigKey: {} };
      this.tenants.push(newTenant);
    } catch (e: unknown) {
      this.logger.error(`create: tenant creation failed`, e as never);
    }
  }
}
