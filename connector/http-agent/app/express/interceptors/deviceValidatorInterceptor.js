const { Logger } = require('@dojot/microservice-sdk');

const logger = new Logger('http-agent');

function createInterceptorMiddleware({ deviceManagerService }) {
  return async (req, res, next) => {
    const { tenant, deviceId } = req.body;

    try {
      const deviceInformation = await deviceManagerService.getDevice(tenant, deviceId);

      if (deviceInformation.disabled) {
        const error = `Device ${deviceId} is disabled. The message will be discarded.`;
        logger.warn(error);
        return res.status(409).json({ error });
      }
      return next();
    } catch (error) {
      return res.status(424).json({ error: error.message });
    }
  };
}

function validateDevice({ deviceManagerService }) {
  return {
    middleware: createInterceptorMiddleware({ deviceManagerService }),
  };
}

module.exports = validateDevice;
