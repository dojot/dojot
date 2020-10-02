const HttpStatus = require('http-status-codes');

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
            const rootCAService = req.scope.resolve('rootCAService');

            const result = await rootCAService.getRootCertificate();

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
            const rootCAService = req.scope.resolve('rootCAService');

            const result = await rootCAService.getRootCRL();

            res.status(HttpStatus.OK).json(result);
          },
        ],
      },
    ],
  };

  return [caRoute, caCrlRoute];
};
