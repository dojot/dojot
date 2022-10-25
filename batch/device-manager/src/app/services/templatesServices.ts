import { Logger } from '@dojot/microservice-sdk';
import { PrismaClient } from '@prisma/client';

export class TemplatesServices {
  constructor(private logger: Logger) {
    this.logger.info('Create Constructor TemplatesServices', {});
  }
}
