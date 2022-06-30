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
        params: identificationService.params,
      };
      try {
        const arg =
          config.authorizationMode === 'basic-auth' ? req : clientCert;
        [req.body.tenant, req.body.deviceId] = await identification[
          config.authorizationMode
        ](arg);
        return next();
      } catch (err) {
        return next(err);
      }
    }

    const reqType = req.path.split('/');

    if (
      config.authorizationMode === 'basic-auth' &&
      reqType[3] === 'unsecure'
    ) {
      try {
        [req.body.tenant, req.body.deviceId] =
          await identificationService.basicAuth(req);
        return next();
      } catch (err) {
        return next(err);
      }
    } 

    if(
      config.authorizationMode === 'params' &&
      reqType[3] === 'unsecure'
    ) {
      try {
        [req.body.tenant, req.body.deviceId] = 
          await identificationService.params(req);
        return next();  
      } catch (error) {
        return next(error)
      }
    }

    BadRequest.message = 'Unable to authenticate';
    return next(BadRequest);
  },
});
