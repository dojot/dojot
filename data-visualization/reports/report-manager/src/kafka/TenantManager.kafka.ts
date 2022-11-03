import { Logger, WebUtils } from '@dojot/microservice-sdk'

import { Config, KafkaPayload } from 'src/types'
import { KafkaUtils, PrismaUtils } from 'src/utils'

export class TenantManager {
  public tenants: WebUtils.TenantInfo[] = []

  constructor(
    private logger: Logger,
    private config: Config,
    private kafkaUtils: KafkaUtils,
    private prismaUtils: PrismaUtils,
    private dojotHttpClient: WebUtils.DojotHttpClient,
  ) {
    this.logger.debug('constructor: tenant manager instantiated', {})
  }

  private updateTenantSchema(tenant: string) {
    const databaseUrl = this.prismaUtils.getDatabaseUrl(tenant)
    this.prismaUtils.deployMigrations(databaseUrl)
    this.prismaUtils.seedDatabase(databaseUrl)
  }

  async update() {
    try {
      this.logger.info('update: updating list of tenants', {})

      const response = await this.dojotHttpClient.request({
        method: 'GET',
        url: this.config.keycloak['tenants.url'],
        timeout: 15000,
      })

      this.logger.info('update: updating the schema of all tenants', {})

      this.tenants = response.data.tenants
      this.tenants.forEach((tenant) => this.updateTenantSchema(tenant.id))

      this.logger.info('update: all schemas have been updated successfully', {})
    } catch (e) {
      this.logger.error('update: failed to update list of tenants', e as never)
    }
  }

  create(payload: KafkaPayload) {
    try {
      const value = this.kafkaUtils.getValue(payload)
      this.logger.info(`create: ${value.type} schema for ${value.tenant}`, {})
      this.updateTenantSchema(value.tenant)
      const newTenant = { id: value.tenant, signatureKey: value.signatureKey }
      this.tenants.push(newTenant as never)
    } catch (e) {
      this.logger.error(`create: tenant creation failed`, e as never)
    }
  }
}
