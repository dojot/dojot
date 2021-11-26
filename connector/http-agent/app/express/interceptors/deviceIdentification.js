const createError = require('http-errors');
const axios = require('axios');
const tls = require('tls');

const Forbidden = new createError[403]();
const BadRequest = new createError[400]();

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
  middleware: async (req, res, next) => {
    if (req.socket instanceof tls.TLSSocket) {
      const clientCert = req.socket.getPeerCertificate();
      try {
        if (
          config.authorizationMode === 'cn' &&
          Object.prototype.hasOwnProperty.call(clientCert, 'subject') &&
          Object.hasOwnProperty.bind(clientCert.subject)('CN')
        ) {
          const { CN } = clientCert.subject;
          const messageKey = CN.split(':');
          [req.tenant, req.deviceId] = messageKey;
          return next();
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
          messageKey = await axios
            .get(
              `http://certificate-acl:3000/internal/api/v1/acl-entries/${fingerprint256}`,
            )
            .then((resp) => resp.data.split(':'));
          [req.tenant, req.deviceId] = messageKey;
          cache.set(fingerprint256, messageKey, config.setTll);
          return next();
        }
      } catch (err) {
        Forbidden.message = `Client certificate is invalid: ${err.message}`;
        return next(Forbidden);
      }
    }

    const reqType = req.path.split('/');

    if (config.unsecureMode && reqType[3] === 'unsecure') {
      const {
        query: { tenant, deviceId },
      } = req;
      [req.tenant, req.deviceId] = [tenant, deviceId];
      return next();
    }

    BadRequest.message = 'Unable to authenticate';
    return next(BadRequest);
  },
});
