const createError = require('http-errors');
const tls = require('tls');

const BadRequest = new createError[400]();

/**
 * A middleware to get device identification from the certificate
 *
 * */
module.exports = ({ config, identificationService }) => ({
  name: 'device-identification-interceptor',
  middleware: async (
    req, res, next,
  ) => {
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

      if (
        config.authorizationMode === 'fingerprint' &&
        Object.prototype.hasOwnProperty.call(clientCert, 'fingerprint256')
      ) {
        const { fingerprint256 } = clientCert;
        let messageKey = cache.get(fingerprint256);
        if (messageKey) {
          [req.tenant, req.deviceId] = messageKey;
          return next();
        }
        try {
          messageKey = await axios
            .get(`http://certificate-acl:3000/internal/api/v1/acl-entries/${fingerprint256}`)
            .then((resp) => resp.data.split(':'));
          [req.tenant, req.deviceId] = messageKey;
          cache.set(
            fingerprint256, messageKey, config.setTll,
          );
          return next();
        } catch (e) {
          err.message =
            'Error trying to get tenant and deviceId in certificate-acl.';
          return next(err);
        }
      }

      err.message = 'Client certificate is invalid';
      return next(err);
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
