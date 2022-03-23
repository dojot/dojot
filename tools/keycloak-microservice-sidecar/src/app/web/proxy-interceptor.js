const proxy = require('express-http-proxy');
const {
  WebUtils : {
    createTokenGen,
  }
} = require('@dojot/microservice-sdk');

function createProxyInterceptor(config, logger, path = '/'){
  return {
    path,
    name: 'keycloak-microservice-sidecar',
    middleware: async (
      req, res, next,
    ) => {
      const newPath = `${config.server.url}${req.url}`;

      if(config.proxy['faketoken.generate']) {
        const tokenGen = createTokenGen();
        const token = await tokenGen.generate({ payload: {}, tenant: req.tenant.id });
        req.headers.authorization = `Bearer ${token}`;
      }
      
      const redirect = proxy(newPath);
      redirect(req, res, next);
    }
  };
};

module.exports = createProxyInterceptor;