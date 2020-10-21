const onFinished = require('on-finished');

const { ServiceUnavailable } = require('../sdk/web/backing/error-template');

/**
 * Create a Lightship Beacon
 *
 * Use beacons to suspend the registered shutdown handler routine
 * when you are processing a request job:
 * https://github.com/gajus/lightship#lightship-usage-examples-beacons
 *
 * @param {object} injected dependencies
 */
module.exports = ({ DIContainer, logger }) => ({
  name: 'beacon-controller',
  middleware: (req, res, next) => {
    const stateManager = DIContainer.resolve('stateManager');

    if (stateManager.lightship.isServerShuttingDown()) {
      const msg = 'Detected that the service is shutting down; '
      + 'No requests will be accepted by this instance anymore';
      logger.error(msg);
      // 503 Service Unavailable
      throw ServiceUnavailable(msg);
    }

    // Beacon is live upon creation. Shutdown handlers are suspended
    // until there are no live beacons
    const beacon = stateManager.createBeacon({
      requestId: req.id,
    });

    onFinished(res, (err) => {
      if (err) {
        logger.error(err);
      }
      // After all Beacons are killed, it is possible
      // to proceed with the shutdown routine
      beacon.die();
    });

    // proceeds to the next controller...
    next();
  },
});
