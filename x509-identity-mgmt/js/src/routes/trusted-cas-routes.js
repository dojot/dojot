const HttpStatus = require('http-status-codes');

const sanitize = require('./sanitize-params');

const { validateNewTrustedCA, validateUpdTrustedCA } = require('../core/schema-validator');

module.exports = ({ mountPoint }) => {
  const caRoute = {
    mountPoint,
    name: 'ca-route',
    path: ['/ca'],
    handlers: [
      {
        /* Root CA Certificate */
        method: 'get',
        middleware: [
          async (req, res) => {
            const caService = req.scope.resolve('trustedCAsService');
            const result = await caService.getRootCertificate();
            res.status(HttpStatus.OK).json(result);
          },
        ],
      },
    ],
  };

  const caCrlRoute = {
    mountPoint,
    name: 'ca-crl-route',
    path: ['/ca/crl'],
    handlers: [
      {
        /* Latest CRL issued by the Root CA */
        method: 'get',
        middleware: [
          async (req, res) => {
            const caService = req.scope.resolve('trustedCAsService');
            const result = await caService.getRootCRL();
            res.status(HttpStatus.OK).json(result);
          },
        ],
      },
    ],
  };

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
          (req, res) => {
            res.sendStatus(201);
          },
        ],
      },
      {
        /* List Trusted CA Certificates */
        method: 'get',
        middleware: [
          (req, res) => {
            res.sendStatus(200);
          },
        ],
      },
    ],
  };

  const trustedCAsCAFingerprintRoute = {
    mountPoint,
    name: 'trusted-cas-fingerprint-route',
    path: ['/trusted-cas/:caCertificateFingerprint'],
    params: [{
      name: 'caCertificateFingerprint',
      trigger: sanitize.fingerprint,
    }],
    handlers: [
      {
        /* Delete Trusted CA Certificate */
        method: 'delete',
        middleware: [
          (req, res) => {
            res.sendStatus(204);
          },
        ],
      },
      {
        /* Update Trusted CA Certificate */
        method: 'patch',
        middleware: [
          validateUpdTrustedCA(),
          (req, res) => {
            res.sendStatus(204);
          },
        ],
      },
      {
        /* Get Trusted CA Certificate */
        method: 'get',
        middleware: [
          (req, res) => {
            res.sendStatus(200);
          },
        ],
      },
    ],
  };

  return [caRoute, caCrlRoute, trustedCAsRoute, trustedCAsCAFingerprintRoute];
};
