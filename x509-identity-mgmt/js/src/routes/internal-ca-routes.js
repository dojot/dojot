const HttpStatus = require('http-status-codes');

const CA_SERVICE = 'internalCAService';

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
            const caService = req.scope.resolve(CA_SERVICE);

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
            const caService = req.scope.resolve(CA_SERVICE);

            const result = await caService.getRootCRL();

            res.status(HttpStatus.OK).json(result);
          },
        ],
      },
    ],
  };

  return [caRoute, caCrlRoute];
};
