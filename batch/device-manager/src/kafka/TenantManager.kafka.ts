import { Logger, WebUtils } from '@dojot/microservice-sdk'

import { KafkaPayload } from 'src/types'
import { KafkaUtils, PrismaUtils } from '../utils/'

export class TenantManager {
  public tenants: WebUtils.TenantInfo[]

  constructor(private logger: Logger) {
    this.tenants = []
  }

  create(payload: KafkaPayload) {
    try {
      const value = KafkaUtils.getValue(payload)
      this.logger.info(`${value.type} bucket for ${value.tenant} tenant`, {})
      const databaseUrl = PrismaUtils.getDatabaseUrl(value.tenant)
      PrismaUtils.deployMigrations(databaseUrl)
    } catch (e: unknown) {
      const error = e as Error
      this.logger.info(`Tenant creation failed: ${error.stack || error}`, {})
    }
  }
}