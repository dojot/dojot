const proxy = require('express-http-proxy');
const {
  WebUtils : {
    createTokenGen,
  },
} = require('@dojot/microservice-sdk');

function createProxyInterceptor(listTenants, config, logger, path = '/'){
  return {
    path,
    name: 'keycloak-microservice-sidecar',
    middleware: async (
      req, res, next,
    ) => {
      const newPath = `${config.server.url}${req.url}`;

      if(config.proxy['token.insert'] === 'legacy') {
        const tokenGen = createTokenGen();
        const token = await tokenGen.generate({ payload: {}, tenant: req.tenant.id });
        req.headers.authorization = `Bearer ${token}`;
      } else if(config.proxy['token.insert'] === 'keycloak') {
        const token = req.tenant.session.getTokenSet().access_token ;
        req.headers.authorization = `Bearer ${token}`;
      }
      
      req.tenant = null;
      const redirect = proxy(newPath);
      redirect(req, res, next);
    }
  };
};

module.exports = createProxyInterceptor;