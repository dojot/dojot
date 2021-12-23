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
        'basic-auth': identificationService.basicAuth,
      };
      try {
        const arg = config.authorizationMode === 'basic-auth' ? req : clientCert;
        [req.tenant, req.deviceId] = await identification[config.authorizationMode](arg);
        return next();
      } catch (err) {
        return next(err);
      }
    }

    const reqType = req.path.split('/');

    if (config.authorizationMode === 'basic-auth' && reqType[3] === 'unsecure') {
      try {
        [req.tenant, req.deviceId] = await identificationService.basicAuth(req);
        return next();
      } catch (err) {
        return next(err);
      }
    }

    BadRequest.message = 'Unable to authenticate';
    return next(BadRequest);
  },
});
