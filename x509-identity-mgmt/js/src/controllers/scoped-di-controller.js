const { asClass, asValue } = require('awilix');

const { Logger } = require('@dojot/microservice-sdk');

// create a scoped DI container
// https://github.com/jeffijoe/awilix#containercreatescope
module.exports = ({ DIContainer }) => ({
  name: 'scoped-di-controller',
  middleware: (req, res, next) => {
    req.scope = DIContainer.createScope();

    // register some request-specific data..
    req.scope.register({
      tenant: asValue(req.tenant),
    });
    req.scope.register({
      logger: asClass(Logger),
    });

    next();
  },
});
