const {
  WebUtils: { framework: { errorTemplate } },
} = require('@dojot/microservice-sdk');
const DIContainer = require('../../DIContainer');

const container = DIContainer();
const logger = container.resolve('logger');

function queryOwnerByFingerprintFromCache(redisManager, fingerprint) {
  return redisManager.getAsync(fingerprint).then((reply) => {
    if (!reply) {
      throw new Error(`No entry in the cache for ${fingerprint}`);
    }
    return reply;
  });
}

async function queryOwnerByFingerprintFromService(fingerprint, serviceConfig, httpCircuit) {
  const options = {
    url: `${serviceConfig.path + fingerprint}?fields=tenant,belongsTo`,
    method: 'GET',
  };
  let response = {};

  try {
    response = await httpCircuit.request(options);
    const {
      tenant,
      belongsTo: { device, application } = {},
    } = response.data;

    const owner = ((tenant && device) ? `${tenant}:${device}` : application);
    if (owner) {
      return owner;
    }
  } catch (requestError) {
    throw errorTemplate.NotFound(
      `The fingerprint ${fingerprint} doesn't exist.`,
    );
  }

  throw errorTemplate.NotFound(
    `The fingerprint ${fingerprint} is not associated with anything.`,
  );
}

function updateFingerprintEntryOnCache(redisManager, fingerprint, value) {
  return redisManager.setAsync(fingerprint, value);
}

module.exports = (redisManager, serviceConfig, httpCircuit) => (fingerprint) => {
  logger.debug(`Getting data for fingerprint ${fingerprint} from cache.`);
  return queryOwnerByFingerprintFromCache(redisManager, fingerprint)
    .then((value) => {
      logger.debug(`Got from cache: ${fingerprint} -> ${value}.`);
      return value;
    }).catch((errorCacheGet) => {
      logger.warn(errorCacheGet);
      logger.debug(
        `Getting data for fingerprint ${fingerprint} from service.`,
      );

      return queryOwnerByFingerprintFromService(fingerprint, serviceConfig, httpCircuit)
        .then((value) => {
          logger.debug(`Got from service: ${fingerprint} -> ${value}.`);

          // Update the cache here might be risky. Suppose the certificate
          // is associated with a device, but the event hasn't be handled by the
          // ACL service, which queries X509 service. Before, the ACL service handles
          // the response, the certificate is disassociated, and the ACL service
          // handles the create and update events. When the response is handled the
          // cache will become inconsistent.
          // To avoid this problem would be necessary to change the cache to expire the
          // entries associated with the devices. It is worth to say that entries
          // associated with applications can't expire because this information is
          // not persisted at X.509 service.
          logger.debug(`Updating Cache: ${fingerprint} -> ${value}.`);
          updateFingerprintEntryOnCache(redisManager, fingerprint, value)
            .then(() => logger.debug(`Updated Cache: ${fingerprint} -> ${value}.`))
            .catch((errorCacheUpdate) => logger.warn(
              `Failed to update cache: ${fingerprint} -> ${value}. ${errorCacheUpdate}`,
            ));

          return value;
        }).catch((errServiceGet) => {
          logger.warn(errServiceGet);
          throw errServiceGet;
        });
    });
};
