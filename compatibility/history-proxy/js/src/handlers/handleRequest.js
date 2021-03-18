const { ConfigManager, Logger } = require('@dojot/microservice-sdk');

const { fetchFromInflux } = require('./utils');

const { unflatten } = require('flat');

const config = unflatten(ConfigManager.getConfig('HISTORYPROXY'));

const logger = new Logger('history-proxy:express/handle/handle-request');

/**
 * Creates parameters for fetching operation and requests
 * it to inFlux.
 *
 * @param {object} pipelineData
 * @param {object} deviceId
 *
 * @return {object} pipelineData + influx's response
 */
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
    headers: { Authorization: r.headers },
    timeout: 30000,
  };
  logger.debug(`Requesting data to Influx with options: ${JSON.stringify(options)}`);
  const resp = await fetchFromInflux(options);

  return { ...r, rawResponse: resp.data };
};


module.exports = { handle };
