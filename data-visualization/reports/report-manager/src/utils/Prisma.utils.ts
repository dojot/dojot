import { execSync } from 'child_process'

import { PrismaClient } from '@prisma/client'
import { Logger } from '@dojot/microservice-sdk'

import { Config } from 'src/types'

export class PrismaUtils {
  constructor(private logger: Logger, private config: Config) {}

  getDatabaseUrl(schema: string) {
    const { user, password, host, port, database } = this.config.postgres
    return `postgresql://${user}:${password}@${host}:${port}/${database}?schema=${schema}`
  }

  deployMigrations(databaseUrl: string) {
    this.logger.debug('deployMigrations: deploying migrations', {})

    execSync('yarn prisma migrate deploy', {
      env: { ...process.env, DATABASE_URL: databaseUrl },
    })

    this.logger.debug('deployMigrations: migrations deployed successfully', {})
  }

  seedDatabase(databaseUrl: string) {
    this.logger.debug('seedDatabase: seeding database', {})

    execSync('yarn prisma db seed', {
      env: { ...process.env, DATABASE_URL: databaseUrl },
    })

    this.logger.debug('seedDatabase: seeds ran successfully', {})
  }

  async disconnectPrisma(tenant: string, prisma: PrismaClient) {
    try {
      this.logger.debug(
        `disconnectPrisma: disconnecting prisma for tenant: ${tenant}`,
        {},
      )
      await prisma.$disconnect()
    } catch (error) {
      this.logger.error(
        `disconnectPrisma: failed to disconnect prisma for tenant: ${tenant}`,
        error as never,
      )
    }
  }

  async dropSchema(tenant: string, prisma: PrismaClient): Promise<boolean> {
    try {
      this.logger.debug(`dropSchema: dropping schema: ${tenant}`, {})

      // WARNING: Susceptible to SQL injections
      // But $executeRawUnsafe does not return any data, just the affected rows
      await prisma.$executeRawUnsafe(`DROP SCHEMA ${tenant} CASCADE`)

      return true
    } catch (error) {
      this.logger.error(
        `dropSchema: failed to drop schema: ${tenant}`,
        error as never,
      )
    }
    return false
  }
}
