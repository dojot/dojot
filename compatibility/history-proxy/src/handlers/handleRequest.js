const { Logger } = require('@dojot/microservice-sdk');
const axios = require('axios');
const configBase = require('../config');

const logger = new Logger('gui-proxy');

const handle = async (r) => {
  let url = `${configBase.retrieverUrl}/tss/v1/devices/${r.deviceId}/attrs/${r.attr}/data`;
  if (r.isAllAttrs) { url = `${configBase.retrieverUrl}/tss/v1/devices/${r.deviceId}/data`; }

  logger.debug(`Requesting data to Influx at: ${url}`);
  const paramList = {
    dateTo: r.dateTo, dateFrom: r.dateFrom, limit: r.limit, order: r.order,
  };
  const pms = new URLSearchParams(paramList);
  const res = await axios.get(
    `${url}?${pms.toString()}`,
    { headers: { authorization: r.headers } },
  );
  return { ...r, rawResponse: res.data };
};

module.exports = { handle };
