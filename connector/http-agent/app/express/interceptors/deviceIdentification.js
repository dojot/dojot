const createError = require('http-errors');
const tls = require('tls');

const BadRequest = new createError[400]();

/**
 * A middleware to get device identification from the certificate
 *
 * */
module.exports = ({ config, identificationService }) => ({
  name: 'device-identification-interceptor',
  middleware: async (req, res, next) => {
    if (req.socket instanceof tls.TLSSocket) {
      const clientCert = req.socket.getPeerCertificate();
      const identification = {
        cn: identificationService.cn,
        fingerprint: identificationService.fingerprint,
      };
      try {
        [req.body.tenant, req.body.deviceId] =
          await identification[config.authorizationMode](clientCert);
        return next();
      } catch (err) {
        return next(err);
      }
    }

    const reqType = req.path.split('/');

    if (config.unsecureMode && reqType[3] === 'unsecure') {
      const {
        query: { tenant, deviceId },
      } = req;
      [req.body.tenant, req.body.deviceId] = [tenant, deviceId];
      return next();
    }

    BadRequest.message = 'Unable to authenticate';
    return next(BadRequest);
  },
});
