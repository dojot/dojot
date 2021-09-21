const util = require('util');
const { pipeline, Writable } = require('stream');

/* eslint-disable security-node/non-literal-reg-expr */
const {
  Logger,
} = require('@dojot/microservice-sdk');

/**
 * Promissify functions.
 */
const pipelineAsync = util.promisify(pipeline);

const logger = new Logger('influxdb-retriever:kafka/DojotConsumer');

class TenancyConsumerLoader {
  static async load(localPersistence, retrieverConsumer) {
    try {
      logger.info('Loading the tenancy topics');
      const tenantSublevels = await localPersistence.initializeLevel('tenants');
      const tenantReadableStream = tenantSublevels[1].createKeyStream();
      const tenantWritableStream = Writable({
        async write(key, encoding, cb) {
          retrieverConsumer.registerCallbacksForDeviceEvents(key);
          cb();
        },
      });

      await pipelineAsync(
        tenantReadableStream,
        tenantWritableStream,
      );
    } catch (error) {
      logger.info('Its not possible to load the tenants.');
      logger.error(error.message);
    }
  }
}

module.exports = TenancyConsumerLoader;
