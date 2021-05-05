const { ConfigManager } = require('@dojot/microservice-sdk');

const { createProxyMiddleware } = require('http-proxy-middleware');

const { unflatten } = require('flat');

const { server: { history } } = unflatten(ConfigManager.getConfig('HISTORYPROXY'));

// Proxy options
const options = {
  target: `${history.protocol}//${history.hostname}:${history.port}`,
  changeOrigin: true,
  pathRewrite: {
    '^/history/notifications/history': '/notifications/history', // rewrite path
  },
};

/**
 * Creates a proxy to handle Notification route
 */
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
