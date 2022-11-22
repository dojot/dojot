import { PrismaClient } from '@prisma/client'
import { Logger, WebUtils } from '@dojot/microservice-sdk'

import { KafkaUtils, PrismaUtils } from 'src/utils'
import { Config, KafkaParsedPayloadValue, KafkaPayload } from 'src/types'

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

  updateTenantSchema(tenant: string) {
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
      this.tenants.forEach((tenant) => {
        this.logger.info(`update: updating schema for ${tenant.id}`, {})
        this.updateTenantSchema(tenant.id)
      })

      this.logger.info('update: all schemas have been updated successfully', {})
    } catch (e) {
      this.logger.error('update: failed to update list of tenants', e as never)
    }
  }

  create({ tenant, signatureKey }: KafkaParsedPayloadValue) {
    try {
      this.logger.info(`create: creating tenant ${tenant}`, {})
      this.updateTenantSchema(tenant)
      const newTenant = { id: tenant, signatureKey, sigKey: {} }
      this.tenants.push(newTenant)
    } catch (e) {
      this.logger.error(`create: tenant creation failed`, e as never)
    }
  }

  async delete({ tenant }: KafkaParsedPayloadValue) {
    try {
      this.logger.info(`delete: deleting tenant ${tenant}`, {})
      this.logger.debug('delete: connecting to the database', {})

      const databaseUrl = this.prismaUtils.getDatabaseUrl(tenant)
      const prisma = new PrismaClient({
        datasources: {
          db: {
            url: databaseUrl,
          },
        },
      })

      const isSchemaDropped = await this.prismaUtils.dropSchema(tenant, prisma)
      await this.prismaUtils.disconnectPrisma(tenant, prisma)

      if (isSchemaDropped) {
        this.logger.debug(`delete: schema ${tenant} dropped`, {})
        this.tenants = this.tenants.filter(({ id }) => id !== tenant)
        this.logger.info(`delete: tenant ${tenant} deleted`, {})
      }
    } catch (e) {
      this.logger.error(`delete: tenant deletion failed`, e as never)
    }
  }

  async handleTenantEvent(payload: KafkaPayload) {
    try {
      const value = this.kafkaUtils.getValue(payload)

      this.logger.info(
        `handleTenantEvent: type (${value.type}), tenant (${value.tenant})`,
        {},
      )

      if (value.type === 'CREATE') this.create(value)
      else if (value.type === 'DELETE') await this.delete(value)
    } catch (e) {
      this.logger.error(
        `handleTenantEvent: tenant event handling failed`,
        e as never,
      )
    }
  }
}
