const { ConfigManager, Logger } = require('@dojot/microservice-sdk');

const http = require('http');

const https = require('https');

require('express-async-errors');

const { unflatten } = require('flat');

const config = unflatten(ConfigManager.getConfig('HISTORYPROXY'));

const logger = new Logger('history-proxy:express/handle/handle-request');

const httpAgent = (config.server.retriever.protocol.startsWith('https')) ? https : http;



const handle = async (r) => {

  let url = `/tss/v1/devices/${r.deviceId}/attrs/${r.attr}/data`;
  if (r.isAllAttrs) {
    url = `/tss/v1/devices/${r.deviceId}/data`;
  }
  const paramList = {
    dateTo: r.dateTo,
    dateFrom: r.dateFrom,
    limit: r.limit,
    order: r.order,
  };
  const pms = new URLSearchParams(paramList);

  const options = {
    protocol: config.server.retriever.protocol,
    host: config.server.retriever.hostname,
    port: config.server.retriever.port,
    path: `${url}?${pms.toString()}`,
    headers: { Authorization: r.headers }
  };
  logger.debug(`Requesting data to Influx with options: ${JSON.stringify(options)}`);

  const resp = await callInflux(options);

  return { ...r, rawResponse: resp.data };
};

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

module.exports = { handle };
