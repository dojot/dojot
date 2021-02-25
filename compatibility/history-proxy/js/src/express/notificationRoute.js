const { ConfigManager } = require('@dojot/microservice-sdk');

const { createProxyMiddleware } = require('http-proxy-middleware');

const { unflatten } = require('flat');


const { server: { history } } = unflatten(ConfigManager.getConfig('HISTORYPROXY'));

// proxy middleware options
const options = {
  target: history.protocol + "//" +
    history.hostname + ":" +
    history.port,
  changeOrigin: true,
  pathRewrite: {
    '^/history/notifications/history': '/notifications/history', // rewrite path
  },
};

const notificationRoute = {
  mountPoint: '/history',
  name: 'notification-route',
  path: ['/notifications/history'],
  handlers: [
    {
      method: 'get',
      middleware: [
        createProxyMiddleware(options),
      ],
    },
  ],
};

module.exports = { notificationRoute };