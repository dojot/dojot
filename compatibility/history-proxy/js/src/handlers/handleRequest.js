const { ConfigManager, Logger } = require('@dojot/microservice-sdk');

const { callInflux } = require('./utils');

require('express-async-errors');

const { unflatten } = require('flat');

const config = unflatten(ConfigManager.getConfig('HISTORYPROXY'));

const logger = new Logger('history-proxy:express/handle/handle-request');


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


module.exports = { handle };
