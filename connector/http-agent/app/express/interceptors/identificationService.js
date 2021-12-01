const createError = require('http-errors');

const Forbidden = new createError[403]();

/**
 * Decodes the certificate that must be sent with the request.
 * Use the fingerprint to consult the certificate-acl
 * get the information if it exists in the CN
 * or authenticates by username and password
 * and add the new tenant request (req.body.tenant and req.body.deviceId).
 *
 * */
module.exports = ({ cache, config, certificateAclService }) => ({
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
      let messageKeyFP = await cache.get(fingerprint256);

      if (messageKeyFP) {
        return messageKeyFP.split(':');
      }

      messageKeyFP = await certificateAclService.getAclEntries(fingerprint256);
      cache.set(fingerprint256, messageKeyFP, config.setTll);
      return messageKeyFP.split(':');
    } catch (e) {
      Forbidden.message = 'Client certificate is invalid';
      throw Forbidden;
    }
  },
});
