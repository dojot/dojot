const HttpStatus = require('http-status-codes');

const { validateRegOrGenCert, validateChangeOwnerCert } = require('../core/schema-validator');

module.exports = ({ mountPoint, db }) => {
  const { certificate: dbCert } = db;

  const certsRoute = {
    mountPoint,
    name: 'certificates-route',
    path: ['/certificates'],
    handlers: [
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
              result = await service.generateCertificate(req.body, req.tenant);
            } else if (req.body.certificatePem) {
              result = await service.registerCertificate(req.body, req.tenant);
            }
            res.status(HttpStatus.CREATED).json(result);
          },
        ],
      },
      {
        /* List x.509 Certificates */
        method: 'get',
        middleware: [
          async (req, res) => {
            const queryFields = dbCert.parseProjectionFields(req.query.fields);
            const filterFields = dbCert.parseConditionFields(req.query, req.tenant);

            const service = req.scope.resolve('certificatesService');

            const { itemCount, results } = await service.listCertificates(
              queryFields, filterFields, req.query.limit, req.offset,
            );
            results.forEach((cert) => dbCert.sanitizeFields(cert));

            const paging = req.getPaging(itemCount);
            res.status(HttpStatus.OK).json({ paging, certificates: results });
          },
        ],
      },
    ],
  };

  const certsFingerprintRoute = {
    mountPoint,
    name: 'certificate-fingerprint-route',
    path: ['/certificates/:certificateFingerprint'],
    handlers: [
      {
        /* Delete x.509 certificate */
        method: 'delete',
        middleware: [
          async (req, res) => {
            const fingerprint = req.params.certificateFingerprint.toUpperCase();
            const queryFields = dbCert.parseProjectionFields(null);
            const filterFields = dbCert.parseConditionFields({ fingerprint }, req.tenant);

            const service = req.scope.resolve('certificatesService');
            const certToRemove = await service.getCertificate(queryFields, filterFields);
            await service.deleteCertificate(certToRemove);
            res.sendStatus(HttpStatus.NO_CONTENT);
          },
        ],
      },
      {
        /* Get x.509 Certificate */
        method: 'get',
        middleware: [
          async (req, res) => {
            const fingerprint = req.params.certificateFingerprint.toUpperCase();
            const queryFields = dbCert.parseProjectionFields(req.query.fields);
            const filterFields = dbCert.parseConditionFields({ fingerprint }, req.tenant);

            const service = req.scope.resolve('certificatesService');
            const result = await service.getCertificate(queryFields, filterFields);
            dbCert.sanitizeFields(result);
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
            const fingerprint = req.params.certificateFingerprint.toUpperCase();
            const filterFields = dbCert.parseConditionFields({ fingerprint }, req.tenant);

            const service = req.scope.resolve('certificatesService');
            await service.changeOwnership(filterFields, req.body.belongsTo);
            res.sendStatus(HttpStatus.NO_CONTENT);
          },
        ],
      },
    ],
  };

  return [certsRoute, certsFingerprintRoute];
};
