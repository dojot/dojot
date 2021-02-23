const HttpStatus = require('http-status-codes');

const CERT_SERVICE = 'certificateService';

const CA_SERVICE = 'internalCAService';

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
              const certService = req.scope.resolve(CERT_SERVICE);
              result = await certService.throwAwayCertificate(req.body);
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

  return [throwAwayRoute, throwAwayCaRoute, throwAwayCaCrlRoute];
};