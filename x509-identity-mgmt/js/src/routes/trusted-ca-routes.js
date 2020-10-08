const HttpStatus = require('http-status-codes');

const sanitize = require('./sanitize-params');

const { validateNewTrustedCA, validateUpdTrustedCA } = require('../core/schema-validator');

const { BadRequest } = require('../sdk/web/backing/error-template');

const CA_SERVICE = 'trustedCAService';

module.exports = ({ mountPoint, trustedCAModel }) => {
  const trustedCAsRoute = {
    mountPoint,
    name: 'trusted-cas-route',
    path: ['/trusted-cas'],
    handlers: [
      {
        /* Register Trusted CA Certificate */
        method: 'post',
        middleware: [
          validateNewTrustedCA(),
          async (req, res) => {
            const pemArr = req.body.caPem.match(sanitize.certRegExp);
            if (!pemArr || pemArr.length > 1) {
              throw BadRequest('Only one CA certificate is expected per request.');
            }

            const [caPem] = pemArr;
            const allowAutoRegistration = req.body.allowAutoRegistration || false;

            const caService = req.scope.resolve(CA_SERVICE);

            const result = await caService.registerCertificate({ caPem, allowAutoRegistration });

            res.status(HttpStatus.CREATED).json(result);
          },
        ],
      },
      {
        /* List Trusted CA Certificates */
        method: 'get',
        middleware: [
          async (req, res) => {
            const queryFields = trustedCAModel.parseProjectionFields(req.query.fields);
            const filterFields = trustedCAModel.parseConditionFields(req.query);

            const caService = req.scope.resolve(CA_SERVICE);

            const { itemCount, results } = await caService.listCertificates(
              queryFields, filterFields, req.query.limit, req.offset,
            );
            results.forEach((cert) => trustedCAModel.sanitizeFields(cert));

            const paging = req.getPaging(itemCount);
            res.status(HttpStatus.OK).json({ paging, 'trusted-cas': results });
          },
        ],
      },
    ],
  };

  const trustedCAsCAFingerprintRoute = {
    mountPoint,
    name: 'trusted-cas-fingerprint-route',
    path: ['/trusted-cas/:caFingerprint'],
    params: [{
      name: 'caFingerprint',
      trigger: sanitize.fingerprint,
    }],
    handlers: [
      {
        /* Get Trusted CA Certificate */
        method: 'get',
        middleware: [
          async (req, res) => {
            const { caFingerprint } = req.params;
            const queryFields = trustedCAModel.parseProjectionFields(req.query.fields);
            const filterFields = trustedCAModel.parseConditionFields({ caFingerprint });

            const caService = req.scope.resolve(CA_SERVICE);

            const result = await caService.getCertificate(queryFields, filterFields);
            trustedCAModel.sanitizeFields(result);

            res.status(HttpStatus.OK).json(result);
          },
        ],
      },
      {
        /* Update Trusted CA Certificate */
        method: 'patch',
        middleware: [
          validateUpdTrustedCA(),
          async (req, res) => {
            const { caFingerprint } = req.params;
            const filterFields = trustedCAModel.parseConditionFields({ caFingerprint });

            const caService = req.scope.resolve(CA_SERVICE);

            await caService.changeAutoRegistration(filterFields, req.body.allowAutoRegistration);

            res.sendStatus(HttpStatus.NO_CONTENT);
          },
        ],
      },
      {
        /* Delete Trusted CA Certificate */
        method: 'delete',
        middleware: [
          async (req, res) => {
            const { caFingerprint } = req.params;
            const queryFields = trustedCAModel.parseProjectionFields(null);
            const filterFields = trustedCAModel.parseConditionFields({ caFingerprint });

            const caService = req.scope.resolve(CA_SERVICE);

            const certToRemove = await caService.getCertificate(queryFields, filterFields);
            await caService.deleteCertificate(certToRemove);

            res.sendStatus(HttpStatus.NO_CONTENT);
          },
        ],
      },
    ],
  };

  return [trustedCAsRoute, trustedCAsCAFingerprintRoute];
};
