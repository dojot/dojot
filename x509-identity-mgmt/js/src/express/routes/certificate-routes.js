const HttpStatus = require('http-status-codes');

const sanitize = require('./sanitize-params');

const CERT_MODEL = 'certificateModel';
const CERT_SERVICE = 'certificateService';

module.exports = ({ mountPoint, schemaValidator }) => {
  const { validateRegOrGenCert, validateChangeOwnerCert } = schemaValidator;

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
            const model = req.scope.resolve(CERT_MODEL);
            const service = req.scope.resolve(CERT_SERVICE);

            const queryFields = model.parseProjectionFields(req.query.fields);
            const filterFields = model.parseConditionFields(req.query);

            const { itemCount, results } = await service.listCertificates(
              queryFields, filterFields, req.query.limit, req.offset,
            );
            results.forEach((cert) => model.sanitizeFields(cert));

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
            const belongsTo = req.body.belongsTo || {};

            const service = req.scope.resolve(CERT_SERVICE);
            if (req.body.csr) {
              result = await service.generateCertificate({ csr: req.body.csr, belongsTo });
            } else if (req.body.certificateChain) {
              const certificateChain = req.body.certificateChain.match(sanitize.certRegExp);
              const caFingerprint = req.body.caFingerprint || '';
              result = await service.registerCertificate({
                caFingerprint, certificateChain, belongsTo,
              });
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
            const model = req.scope.resolve(CERT_MODEL);
            const service = req.scope.resolve(CERT_SERVICE);

            const { fingerprint } = req.params;
            const queryFields = model.parseProjectionFields(req.query.fields);
            const filterFields = model.parseConditionFields({ fingerprint });

            const result = await service.getCertificate(queryFields, filterFields);
            model.sanitizeFields(result);

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
            const model = req.scope.resolve(CERT_MODEL);
            const service = req.scope.resolve(CERT_SERVICE);

            const { fingerprint } = req.params;
            const filterFields = model.parseConditionFields({ fingerprint });

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
            const model = req.scope.resolve(CERT_MODEL);
            const service = req.scope.resolve(CERT_SERVICE);

            const { fingerprint } = req.params;
            const queryFields = model.parseProjectionFields(null);
            const filterFields = model.parseConditionFields({ fingerprint });

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
