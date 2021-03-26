const HttpStatus = require('http-status-codes');

const { ContentType, contentNegotiation } = require('./contentNegotiation');

const sanitizeParams = require('./sanitizeParams');

const CERT_SERVICE = 'certificateService';

const CA_SERVICE = 'internalCAService';

const TRUSTED_CA_SERVICE = 'trustedCAService';

const accepts = [ContentType.pem, ContentType.json];
const contentDispositionHeader = 'Content-Disposition';

function servePem(res, status, filename, content) {
  res.set(contentDispositionHeader, `attachment; filename="${filename}"`);
  res.status(status).send(
    Buffer.from(content),
  );
}

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
          async (req, res, next) => {
            if (req.body.csr) {
              const csr = sanitizeParams.sanitizeLineBreaks(req.body.csr);
              const belongsTo = req.body.belongsTo || {};

              const certService = req.scope.resolve(CERT_SERVICE);
              res.result = await certService.throwAwayCertificate({
                csr, belongsTo,
              });
              next();
            } else {
              throw BadRequest('It is necessary to inform the CSR for the certificate to be issued.');
            }
          },
          contentNegotiation(accepts, {
            'application/x-pem-file': (res) => servePem(res, HttpStatus.CREATED, 'cert.pem', res.result.certificatePem),
            'application/json': (res) => {
              res.status(HttpStatus.CREATED).json(res.result);
            },
          }),
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
          async (req, res, next) => {
            const caService = req.scope.resolve(CA_SERVICE);
            res.result = await caService.getRootCertificate();
            next();
          },
          contentNegotiation(accepts, {
            'application/x-pem-file': (res) => servePem(res, HttpStatus.OK, 'ca.pem', res.result.caPem),
            'application/json': (res) => {
              res.status(HttpStatus.OK).json(res.result);
            },
          }),
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
            res.bundle = [caPem, ...trustedBundle];
            next();
          },
          contentNegotiation(accepts, {
            'application/x-pem-file': (res) => servePem(res, HttpStatus.OK, 'cabundle.pem', res.bundle.join('\n')),
            'application/json': (res) => {
              res.status(HttpStatus.OK).json(res.bundle);
            },
          }),
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
          async (req, res, next) => {
            const caService = req.scope.resolve(CA_SERVICE);
            res.result = await caService.getRootCRL(req.query.update === 'true');
            next();
          },
          contentNegotiation(accepts, {
            'application/x-pem-file': (res) => servePem(res, HttpStatus.OK, 'crl.pem', res.result.crl),
            'application/json': (res) => {
              res.status(HttpStatus.OK).json(res.result);
            },
          }),
        ],
      },
    ],
  };

  return [throwAwayRoute, throwAwayCaRoute, throwAwayTrustedCAsRoute, throwAwayCaCrlRoute];
};
