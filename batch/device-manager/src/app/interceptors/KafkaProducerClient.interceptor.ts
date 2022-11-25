import { Logger } from '@dojot/microservice-sdk';
import { NextFunction, Request, Response } from 'express';

import { KafkaProducer } from 'src/kafka/kafka-producer';
import { AppConfig, DojotSdkInterceptor } from 'src/types';

export abstract class KafkaProducerClientInterceptor {
  static use(
    logger: Logger,
    config: AppConfig,
    kafkaproducer: KafkaProducer,
  ): DojotSdkInterceptor {
    const middleware = async (
      req: Request,
      res: Response,
      next: NextFunction,
    ) => {
      const isConnected = await kafkaproducer.isConnected();

      if (!isConnected) {
        logger.debug(
          'KafkaProducerClientInterceptor - Error Service Unavailable  ',
          {},
        );
        return res.status(503).send();
      }

      return next();
    };

    return {
      path: ['/'],
      name: 'KafkaProducerClientInterceptor',
      middleware: [middleware],
    };
  }
}
