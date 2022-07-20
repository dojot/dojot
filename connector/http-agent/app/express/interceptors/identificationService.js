const createError = require('http-errors');

const Forbidden = new createError[403]();
const Unauthorized = new createError[401]();


/**
 * Decodes the certificate that must be sent with the request.
 * Use the fingerprint to consult the certificate-acl
 * get the information if it exists in the CN
 * or authenticates by username and password
 * and add the new tenant request (req.tenant and req.deviceId).
 *
 * */
module.exports = ({
  redisManager, deviceAuthService, certificateAclService,
}) => ({
  cn: async (clientCert) => {
    try {
      const { CN } = clientCert.subject;
      return CN.split(':');
    } catch (e) {
      Forbidden.message = 'Client certificate is invalid';
      throw Forbidden;
    }
  },
  fingerprint: async (clientCert) => {
    try {
      const { fingerprint256 } = clientCert;
      let messageKeyFP = await redisManager.getAsync(fingerprint256);

      if (messageKeyFP) {
        return messageKeyFP.split(':');
      }

      messageKeyFP = await certificateAclService.getAclEntries(fingerprint256);
      redisManager.setAsync(fingerprint256, messageKeyFP);
      return messageKeyFP.split(':');
    } catch (e) {
      Forbidden.message = 'Client certificate is invalid';
      throw Forbidden;
    }
  },
  basicAuth: async (req) => {
    try {
      let authHeader;
      let username;
      let password;
      let tenant;
      let deviceId;
      try {
        authHeader = req.headers.authorization.split(' ');
        // eslint-disable-next-line new-cap
        const buff = new Buffer.from(authHeader[1], 'base64');
        const text = buff.toString('ascii');

        [username, password] = text.split(':');
        [tenant, deviceId] = username.split('@');
      } catch (err) {
        throw new Error('Invalid Basic token.');
      }

      const messageKeyDA = await redisManager.getSecurity(username, password);
      if (messageKeyDA) {
        return [tenant, deviceId];
      }

      try {
        const authenticated = await deviceAuthService.getAuthenticationStatus(
          tenant, username, password,
        );
        if (authenticated) {
          await redisManager.setSecurity(username, password);
          return [tenant, deviceId];
        }
      } catch (err) {
        throw new Error('Invalid credentials.');
      }

      throw new Error('Invalid credentials.');
    } catch (err) {
      Unauthorized.message = err.message;
      throw Unauthorized;
    }
  },
});
