const HttpStatus = require('http-status-codes');

const sanitize = require('./sanitize-params');

const { validateRegOrGenCert, validateChangeOwnerCert } = require('../core/schema-validator');

module.exports = ({ mountPoint, certificateModel }) => {
  const certsRoute = {
    mountPoint,
    name: 'certificates-route',
    path: ['/certificates'],
    handlers: [
      {
        /* List x.509 Certificates */
        method: 'get',
        middleware: [
          async (req, res) => {
            const queryFields = certificateModel.parseProjectionFields(req.query.fields);
            const filterFields = certificateModel.parseConditionFields(req.query);

            const service = req.scope.resolve('certificatesService');

            const { itemCount, results } = await service.listCertificates(
              queryFields, filterFields, req.query.limit, req.offset,
            );
            results.forEach((cert) => certificateModel.sanitizeFields(cert));

            const paging = req.getPaging(itemCount);
            res.status(HttpStatus.OK).json({ paging, certificates: results });
          },
        ],
      },
      {
        /* Generate x.509 Certificate from CSR
         * (or also)
         * Register x.509 Certificate Issued by an External CA */
        method: 'post',
        middleware: [
          validateRegOrGenCert(),
          async (req, res) => {
            let result = null;
            if (!req.body.belongsTo) {
              req.body.belongsTo = {};
            }
            const service = req.scope.resolve('certificatesService');
            if (req.body.csr) {
              result = await service.generateCertificate(req.body);
            } else if (req.body.certificatePem) {
              result = await service.registerCertificate(req.body);
            }
            res.status(HttpStatus.CREATED).json(result);
          },
        ],
      },
    ],
  };

  const certsFingerprintRoute = {
    mountPoint,
    name: 'certificate-fingerprint-route',
    path: ['/certificates/:fingerprint'],
    params: [{
      name: 'fingerprint',
      trigger: sanitize.fingerprint,
    }],
    handlers: [
      {
        /* Get x.509 Certificate */
        method: 'get',
        middleware: [
          async (req, res) => {
            const { fingerprint } = req.params;
            const queryFields = certificateModel.parseProjectionFields(req.query.fields);
            const filterFields = certificateModel.parseConditionFields({ fingerprint });

            const service = req.scope.resolve('certificatesService');

            const result = await service.getCertificate(queryFields, filterFields);
            certificateModel.sanitizeFields(result);

            res.status(HttpStatus.OK).json(result);
          },
        ],
      },
      {
        /* Change the Ownership of a Specified x.509 Certificate */
        method: 'patch',
        middleware: [
          validateChangeOwnerCert(),
          async (req, res) => {
            const { fingerprint } = req.params;
            const filterFields = certificateModel.parseConditionFields({ fingerprint });

            const service = req.scope.resolve('certificatesService');

            await service.changeOwnership(filterFields, req.body.belongsTo);

            res.sendStatus(HttpStatus.NO_CONTENT);
          },
        ],
      },
      {
        /* Delete x.509 certificate */
        method: 'delete',
        middleware: [
          async (req, res) => {
            const { fingerprint } = req.params;
            const queryFields = certificateModel.parseProjectionFields(null);
            const filterFields = certificateModel.parseConditionFields({ fingerprint });

            const service = req.scope.resolve('certificatesService');

            const certToRemove = await service.getCertificate(queryFields, filterFields);
            await service.deleteCertificate(certToRemove);

            res.sendStatus(HttpStatus.NO_CONTENT);
          },
        ],
      },
    ],
  };

  return [certsRoute, certsFingerprintRoute];
};
