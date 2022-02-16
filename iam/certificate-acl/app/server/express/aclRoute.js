const HTTPStatus = require('http-status-codes');

const DIContainer = require('../../DIContainer');

const container = DIContainer();
const logger = container.resolve('logger');

/**
 * Routes to ACL
 *
 * @param {Promise< value | error>} queryOwnerByFingerprint method to query an ACL entry
 */
module.exports = (queryOwnerByFingerprint) => {
  /**
    * Creates the middleware to handle ACL route
    */
  const aclRoute = {
    mountPoint: '/internal/api/v1',
    name: 'acl-fingerprint-route',
    path: ['/acl-entries/:fingerprint'],
    handlers: [
      {
        method: 'get',
        middleware: [
          async (req, res) => {
            logger.debug(`route.get: req.params=${JSON.stringify(req.params)} `
                + `req.query=${JSON.stringify(req.query)}`);
            const { fingerprint } = req.params;
            return queryOwnerByFingerprint(fingerprint).then((value) => {
              res.status(HTTPStatus.OK).json(value);
            });
          },
        ],
      },
    ],
  };

  return aclRoute;
};
