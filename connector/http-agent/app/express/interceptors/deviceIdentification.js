const createError = require('http-errors');
const tls = require('tls');

/**
 * Function to decode basic authentication base64
 *
 * */
const decodeBasic = (req) => {
  try {
    const authHeader = req.headers.authorization.split(' ');
    // eslint-disable-next-line new-cap
    const buff = new Buffer.from(authHeader[1], 'base64');
    const text = buff.toString('ascii');

    const [username, password] = text.split(':');
    const [tenant, deviceId] = username.split('@');

    return [authHeader[1], username, password, tenant, deviceId];
  } catch (err) {
    throw new Error('Invalid Basic token.');
  }
};

/**
 * A middleware to get device identification from the certificate
 *
 * Decodes the certificate that must be sent with the request.
 * Use the fingerprint to consult the certificate-acl
 * or get the information if it exists in the CN
 * and add the new tenant request (req.tenant and req.deviceId).
 *
 * */
module.exports = ({
  redisManager, deviceAuthService, certificateAclService, config,
}) => ({
  name: 'device-identification-interceptor',
  middleware: async (req, res, next) => {
    const Forbidden = new createError[403]();
    const Unauthorized = new createError[401]();
    const BadRequest = new createError[400]();

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
          Forbidden.message =
            'Error trying to get tenant and deviceId in CN of certificate.';
          return next(Forbidden);
        }
      }

      if (
        config.authorizationMode === 'fingerprint' &&
        Object.prototype.hasOwnProperty.call(clientCert, 'fingerprint256')
      ) {
        try {
          const { fingerprint256 } = clientCert;
          let messageKeyFP = await redisManager.getAsync(fingerprint256);

          if (messageKeyFP) {
            [req.tenant, req.deviceId] = messageKeyFP.split(':');
            return next();
          }

          messageKeyFP = await certificateAclService.getAclEntries(fingerprint256);
          [req.tenant, req.deviceId] = messageKeyFP.split(':');

          redisManager.setAsync(fingerprint256, messageKeyFP);
          return next();
        } catch (e) {
          Forbidden.message =
            'Error trying to get tenant and deviceId in certificate-acl.';
          return next(Forbidden);
        }
      }

      if (config.authorizationMode === 'basic-auth') {
        try {
          const [authHeader, username, password, tenant, deviceId] = decodeBasic(req);

          const messageKeyDA = await redisManager.getAsync(authHeader);
          if (messageKeyDA) {
            [req.tenant, req.deviceId] = [tenant, deviceId];
            return next();
          }

          try {
            const authenticated = await deviceAuthService
              .getAuthenticationStatus(username, password);
            if (authenticated) {
              [req.tenant, req.deviceId] = [tenant, deviceId];
              await redisManager.setAsync(authHeader, true);
              return next();
            }
          } catch (err) {
            throw new Error('Error trying to get tenant and deviceId in basic-auth.');
          }

          throw new Error('Invalid credentials.');
        } catch (err) {
          Unauthorized.message = err.message;
          return next(Unauthorized);
        }
      }
    }

    const reqType = req.path.split('/');

    if (config.authorizationMode === 'basic-auth' && reqType[3] === 'unsecure') {
      try {
        const [authHeader, username, password, tenant, deviceId] = decodeBasic(req);

        const messageKeyDA = await redisManager.getAsync(authHeader);
        if (messageKeyDA) {
          [req.tenant, req.deviceId] = [tenant, deviceId];
          return next();
        }

        try {
          const authenticated = await deviceAuthService
            .getAuthenticationStatus(username, password);
          if (authenticated) {
            [req.tenant, req.deviceId] = [tenant, deviceId];
            await redisManager.setAsync(authHeader, true);
            return next();
          }
        } catch (err) {
          throw new Error('Error trying to get tenant and deviceId in basic-auth.');
        }

        throw new Error('Invalid credentials.');
      } catch (err) {
        Unauthorized.message = err.message;
        return next(Unauthorized);
      }
    }

    BadRequest.message = 'Unable to authenticate';
    return next(BadRequest);
  },
});
