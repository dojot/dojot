const http = require('http');
const HTTPStatus = require('http-status-codes');

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

function queryOwnerByFingerprintFromService(fingerprint, serviceConfig) {
  return new Promise((resolve, reject) => {
    const options = {
      hostname: serviceConfig.hostname,
      port: serviceConfig.port,
      path: `${serviceConfig.path + fingerprint}?fields=tenant,belongsTo`,
      method: 'GET',
      timeout: serviceConfig.timeout,
    };
    const req = http.request(options, (res) => {
      if (res.statusCode === HTTPStatus.OK) {
        let data = '';
        // A chunk of data has been received.
        res.on('data', (chunk) => {
          data += chunk;
        });

        // The whole response has been received.
        res.on('end', () => {
          const {
            tenant,
            belongsTo: { device, application } = {},
          } = JSON.parse(data);
          const owner = ((tenant && device) ? `${tenant}:${device}` : application);
          // Notice that value will be an empty string if
          // the certificate is not associated with anything
          if (owner) {
            resolve(owner);
          } else {
            reject(errorTemplate.NotFound(
              `The fingerprint ${fingerprint} is not associated with anything.`,
            ));
          }
        });
      } else if (res.statusCode === HTTPStatus.NOT_FOUND) {
        reject(errorTemplate.NotFound(
          `The fingerprint ${fingerprint} doesn't exist.`,
        ));
      } else {
        reject(new Error(`Failed to get fingerprint ${fingerprint} from service`
        + ` (HTTP Status = ${res.statusCode})`));
      }
    });

    req.on('timeout', () => {
      req.destroy();
      reject(new Error(`Failed to get fingerprint ${fingerprint} from service`
      + ' (request timeout)'));
    });

    req.on('error', (err) => {
      reject(new Error(`Failed to get fingerprint ${fingerprint} from service`
      + ` (${err})`));
    });

    req.end();
  });
}

function updateFingerprintEntryOnCache(redisManager, fingerprint, value) {
  return redisManager.setAsync(fingerprint, value);
}

module.exports = (redisManager, serviceConfig) => (fingerprint) => {
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

      return queryOwnerByFingerprintFromService(fingerprint, serviceConfig)
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
