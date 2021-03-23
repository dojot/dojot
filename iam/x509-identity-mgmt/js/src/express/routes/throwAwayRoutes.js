const HttpStatus = require('http-status-codes');

const sanitizeParams = require('./sanitizeParams');

const CERT_SERVICE = 'certificateService';

const CA_SERVICE = 'internalCAService';

const TRUSTED_CA_SERVICE = 'trustedCAService';

module.exports = ({ mountPoint, schemaValidator, errorTemplate }) => {
  const { validateRegOrGenCert } = schemaValidator;
  const { BadRequest } = errorTemplate;

  const throwAwayRoute = {
    mountPoint,
    name: 'throw-away-route',
    path: ['/throw-away'],
    handlers: [
      {
        /* issue and throw away a certificate given a CSR.
         * Used only by services behind the API gateway */
        method: 'post',
        middleware: [
          validateRegOrGenCert(),
          async (req, res) => {
            let result = null;
            if (req.body.csr) {
              const csr = sanitizeParams.sanitizeLineBreaks(req.body.csr);
              const belongsTo = req.body.belongsTo || {};

              const certService = req.scope.resolve(CERT_SERVICE);
              result = await certService.throwAwayCertificate({
                csr, belongsTo,
              });
            } else {
              throw BadRequest('It is necessary to inform the CSR for the certificate to be issued.');
            }
            res.status(HttpStatus.CREATED).json(result);
          },
        ],
      },
    ],
  };

  const throwAwayCaRoute = {
    mountPoint,
    name: 'throw-away-ca-route',
    path: ['/throw-away/ca'],
    handlers: [
      {
        /* retrieves the certificate from the root CA without needing the JWT token.
         * Used only by services behind the API gateway */
        method: 'get',
        middleware: [
          async (req, res) => {
            const caService = req.scope.resolve(CA_SERVICE);
            const result = await caService.getRootCertificate();
            res.status(HttpStatus.OK).json(result);
          },
        ],
      },
    ],
  };

  const throwAwayTrustedCAsRoute = {
    mountPoint,
    name: 'throw-away-trustedcas-route',
    path: ['/throw-away/ca/bundle'],
    handlers: [
      {
        /* retrieves the certificate from the dojot Root CA and all the
         * other trusted CAs without needing the JWT token.
         * Used only by services behind the API gateway */
        method: 'get',
        middleware: [
          async (req, res, next) => {
            const caService = req.scope.resolve(CA_SERVICE);
            const trustedCaService = req.scope.resolve(TRUSTED_CA_SERVICE);

            const { caPem } = await caService.getRootCertificate();
            const trustedBundle = await trustedCaService.getCertificateBundle();

            // The bundle will always contain the platform's internal CA certificate first
            const bundle = [caPem, ...trustedBundle];

            res.bundle = bundle;
            next();
          },
          (req, res) => {
            // the order of this list is significant; should be server preferred order
            switch (req.accepts(['application/x-pem-file', 'application/json'])) {
              case 'application/x-pem-file':
                res.set('Content-Type', 'application/x-pem-file; charset=utf-8');
                res.set('Content-Disposition', 'attachment; filename="trustedca_bundle.pem"');
                res.status(HttpStatus.OK).send(
                  Buffer.from(res.bundle.join('\n')),
                );
                break;
              case 'application/json':
                res.set('Content-Type', 'application/json; charset=utf-8');
                res.status(HttpStatus.OK).json(res.bundle);
                break;
              default:
                res.sendStatus(HttpStatus.NOT_ACCEPTABLE);
            }
          },
        ],
      },
    ],
  };

  const throwAwayCaCrlRoute = {
    mountPoint,
    name: 'throw-away-ca-crl-route',
    path: ['/throw-away/ca/crl'],
    handlers: [
      {
        /* Latest CRL issued by the Root CA without needing the JWT token.
         * Used only by services behind the API gateway */
        method: 'get',
        middleware: [
          async (req, res) => {
            const caService = req.scope.resolve(CA_SERVICE);
            const result = await caService.getRootCRL(req.query.update === 'true');
            res.status(HttpStatus.OK).json(result);
          },
        ],
      },
    ],
  };

  return [throwAwayRoute, throwAwayCaRoute, throwAwayTrustedCAsRoute, throwAwayCaCrlRoute];
};
