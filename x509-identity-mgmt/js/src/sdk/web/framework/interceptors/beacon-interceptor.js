const onFinished = require('on-finished');

const { ServiceUnavailable } = require('../backing/error-template');

/**
 * Create a Lightship Beacon
 *
 * Use beacons to suspend the registered shutdown handler routine
 * when you are processing a request job:
 * https://github.com/gajus/lightship#lightship-usage-examples-beacons
 */
function createInterceptor(stateManager, logger, path = '/') {
  return {
    path,
    name: 'beacon-interceptor',
    middleware: (req, res, next) => {
      if (stateManager.lightship.isServerShuttingDown()) {
        const msg = 'Detected that the service is shutting down; '
        + 'No requests will be accepted by this instance anymore';
        logger.error(msg);
        // 503 Service Unavailable
        throw ServiceUnavailable(msg);
      }

      // Beacon is live upon creation. Shutdown handlers are suspended
      // until there are no live beacons
      const beacon = stateManager.createBeacon({ requestId: req.id });

      logger.debug('Incoming request:', { id: req.id });

      onFinished(res, (err) => {
        if (err) {
          logger.error(err);
        }
        // After all Beacons are killed, it is possible
        // to proceed with the shutdown routine
        beacon.die();

        logger.debug('request has been finished:', { id: req.id });
      });

      // proceeds to the next middleware...
      next();
    },
  };
}

module.exports = ({ stateManager, logger, path }) => createInterceptor(stateManager, logger, path);
