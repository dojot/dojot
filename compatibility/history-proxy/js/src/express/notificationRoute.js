const { ConfigManager, Logger } = require('@dojot/microservice-sdk');

const { createProxyMiddleware } = require('http-proxy-middleware');

const axios = require('axios');

const { unflatten } = require('flat');

const logger = new Logger('history-proxy:express/routes');

const { server: { history } } = unflatten(ConfigManager.getConfig('HISTORYPROXY'));

const httpProxy = require('express-http-proxy');


//framework.use('/history/notifications/history', createProxyMiddleware({ target: 'http://www.example.org', changeOrigin: true }));


const notificationRoute = {
  mountPoint: '/history',
  name: 'notification-route',
  path: ['/notifications/history'],
  handlers: [
    {
      method: 'get',
      middleware: [
        createProxyMiddleware({
          target: history.protocol + "//" +
            history.hostname + ":" +
            history.port
          , changeOrigin: true
        }),
       
      ],
    },
  ],
};

module.exports = { notificationRoute };

 /* number async (req, res, next) => {
           logger.info('Notifications were requested.');
           logger.debug(`route.get: req.query=${JSON.stringify(req.query)}`);
 
           console.log(notificationProxy(req, res, next));
 
           //          res.redirect('/to');
           /*
           
                     const url = `${history.protocol}//${history.hostname}:${history.port}/history/notifications/history`;
                     const pms = new URLSearchParams(req.query);
           
                     console.log("---", `${url}?${pms.toString()}`,
                       { headers: req.headers });
                     axios.get(
                       `${url}?${pms.toString()}`,
                       { headers: req.headers },
                     ).then((r) => {
           
                       logger.debug(`Sending response: ${JSON.stringify(r)}`);
                       res.status(r.code).send(r.data);
           
                     }).catch((error) => {
           
                       logger.error(error);
                       const code = error.response.status;
                       const title = error.response.statusText;
                       const description = error.response.data.error;
           
                       return res.status(code).send({
                         title,
                         description,
                       });
                     });
                     */
                    