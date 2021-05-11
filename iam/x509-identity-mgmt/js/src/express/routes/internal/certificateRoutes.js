const HttpStatus = require('http-status-codes');

const sanitizeParams = require('../sanitizeParams');

const CERT_MODEL = 'certificateModel';
const CERT_SERVICE = 'certificateService';

module.exports = ({ mountPoint }) => {
  const internalCertsRoute = {
    mountPoint,
    name: 'internal-certificates-route',
    path: ['/certificates'],
    handlers: [
      {
        /* List x.509 Certificates */
        method: 'get',
        middleware: [
          async (req, res) => {
            const model = req.scope.resolve(CERT_MODEL);
            const service = req.scope.resolve(CERT_SERVICE);

            const queryFields = model.parseProjectionFields(req.query.fields);
            const filterFields = model.parseConditionFields(req.query);

            // As access to this route is platform exclusively internal use,
            // we can elevate service privileges to bypass certain restrictions.
            service.elevatePrivileges();

            const { itemCount, results } = await service.listCertificates(
              queryFields, filterFields, req.query.limit, req.offset,
            );

            // It is important to drop privileges right after performing the operation
            service.dropPrivileges();

            results.forEach((cert) => model.sanitizeFields(cert));

            const paging = req.getPaging(itemCount);
            res.status(HttpStatus.OK).json({ paging, certificates: results });
          },
        ],
      },
    ],
  };

  const internalCertFingerprintRoute = {
    mountPoint,
    name: 'internal-certificate-fingerprint-route',
    path: ['/certificates/:fingerprint'],
    params: [{
      name: 'fingerprint',
      trigger: sanitizeParams.fingerprintHandler,
    }],
    handlers: [
      {
        /* Get x.509 Certificate */
        method: 'get',
        middleware: [
          async (req, res) => {
            const model = req.scope.resolve(CERT_MODEL);
            const service = req.scope.resolve(CERT_SERVICE);

            const { fingerprint } = req.params;
            const queryFields = model.parseProjectionFields(req.query.fields);
            const filterFields = model.parseConditionFields({ fingerprint });

            // As access to this route is platform exclusively internal use,
            // we can elevate service privileges to bypass certain restrictions.
            service.elevatePrivileges();

            const result = await service.getCertificate(queryFields, filterFields);

            // It is important to drop privileges right after performing the operation
            service.dropPrivileges();

            model.sanitizeFields(result);

            res.status(HttpStatus.OK).json(result);
          },
        ],
      },
    ],
  };

  return [internalCertsRoute, internalCertFingerprintRoute];
};
