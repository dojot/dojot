
const { ConfigManager, Logger } = require('@dojot/microservice-sdk');

const http = require('http');

const https = require('https');

const { unflatten } = require('flat');

const config = unflatten(ConfigManager.getConfig('HISTORYPROXY'));

const httpAgent = (config.server.retriever.protocol.startsWith('https')) ? https : http;

const logger = new Logger('history-proxy:express/handle/handle-request');



const callInflux = async (options) => {
  return new Promise((resolve, reject) => {
    const request = httpAgent.get(options, (response) => {
      let rawData = '';
      response.on('data', (chunk) => {
        rawData += chunk;
      });
      response.on('end', () => {
        try {
          if (response.statusCode === 200) {
            const data = JSON.parse(rawData);
            resolve(data);
          } else {
            resolve(null);
          }
        } catch (ex) {
          logger.error('Connection error', ex);
          reject(ex);
        }
      });
      request.on('error', (ex) => {
        logger.error('Connection error', ex);
        reject(ex);
      });
    });
  });
}

module.exports = { callInflux };

