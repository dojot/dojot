import * as crypto from 'crypto';

import { Logger } from '@dojot/microservice-sdk';
import { PrismaClient } from '@prisma/client';

import { AppConfig } from 'src/types';

export class PrismaUtils {
  constructor(private logger: Logger, private appconfig: AppConfig) {}

  getDatabaseUrl = (schema: string, logger: Logger, config: AppConfig) => {
    if (schema.length == 0) {
      schema = config.database.schema;
    }
    const user = config.database.user;
    const password = config.database.password;
    const host = config.database.host;
    const port = config.database.port;
    const database = config.database.name;
    const url = `postgresql://${user}:${password}@${host}:${port}/${database}?schema=${schema}`;
    logger.debug('Connection with Database', { url });
    return url;
  };

  async disconnectPrisma(tenant: string, prisma: PrismaClient) {
    try {
      this.logger.debug(
        `disconnectPrisma: disconnecting prisma for tenant: ${tenant}`,
        {},
      );
      await prisma.$disconnect();
    } catch (error) {
      this.logger.error(
        `disconnectPrisma: failed to disconnect prisma for tenant: ${tenant}`,
        error as never,
      );
    }
  }

  getRandomicHexIdDevices(size_byte = 3) {
    const randomString = crypto.randomBytes(size_byte).toString('hex');
    return randomString;
  }
}
