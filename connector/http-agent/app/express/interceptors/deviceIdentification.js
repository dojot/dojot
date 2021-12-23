const createError = require('http-errors');
const axios = require('axios');
const tls = require('tls');

/**
 * A middleware to get device identification from the certificate
 *
 * Decodes the certificate that must be sent with the request.
 * Use the fingerprint to consult the certificate-acl
 * or get the information if it exists in the CN
 * and add the new tenant request (req.tenant and req.deviceId).
 *
 * */
module.exports = ({ cache, config }) => ({
  name: 'device-identification-interceptor',
  middleware: async (
    req, res, next,
  ) => {
    const err = new createError.Unauthorized();

    if (req.socket instanceof tls.TLSSocket) {
      const clientCert = req.socket.getPeerCertificate();
      if (
        config.authorizationMode === 'cn' &&
        Object.prototype.hasOwnProperty.call(clientCert, 'subject') &&
        Object.hasOwnProperty.bind(clientCert.subject)('CN')
      ) {
        const { CN } = clientCert.subject;
        try {
          const messageKey = CN.split(':');
          [req.tenant, req.deviceId] = messageKey;
          return next();
        } catch (e) {
          err.message =
            'Error trying to get tenant and deviceId in CN of certificate.';
          return next(err);
        }
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

    if (config.unsecureMode && reqType[3] === 'unsecure') {
      const {
        query: { tenant, deviceId },
      } = req;
      [req.tenant, req.deviceId] = [tenant, deviceId];
      return next();
    }

    err.message = 'Missing client certificate';
    return next(err);
  },
});
