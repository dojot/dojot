const createError = require('http-errors');

/**
 * A middleware to validate device
 *
 * Checks the existence of the device
 *
 * */
module.exports = ({ deviceService }) => ({
  path: '/basic-auth/v1/:deviceId/basic-credentials',
  name: 'device-validation-interceptor',
  middleware: async (req, res, next) => {
    const err = new createError.BadRequest();

    const { tenant, params: { deviceId } } = req;

    try {
      const isValidDevice = await deviceService.validDevice(tenant, deviceId);
      if (!isValidDevice) {
        err.message = 'Device is not valid.';
        return next(err);
      }
      return next();
    } catch (error) {
      err.message = 'Device is not valid.';
      return next(err);
    }
  },
});
