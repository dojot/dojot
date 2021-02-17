const { Logger } = require('@dojot/microservice-sdk');
const createError = require('http-errors');

const logger = new Logger('gui-proxy');

const parseOneAttr = (deviceId, attr, data) => data.map((element) => {
  const newEl = { device_id: deviceId };
  newEl.ts = element.ts;
  newEl.value = element.value;
  newEl.attr = attr;
  newEl.metadata = {};
  return newEl;
});

const parseAllAttr = (deviceId, data) => {
  const objRtn = {};
  data.forEach((element) => {
    element.attrs.forEach((attr) => {
      const newEl = { device_id: deviceId };
      const { value, label } = attr;
      newEl.ts = element.ts;
      newEl.value = value;
      newEl.attr = label;
      newEl.metadata = {};
      if (objRtn[label]) {
        objRtn[label].push(newEl);
      } else {
        objRtn[label] = [newEl];
      }
    });
  });
  return objRtn;
};

const handle = (r) => {
  const { rawResponse, deviceId, attr } = r;
  logger.debug(`Received data: ${JSON.stringify(rawResponse)}`);

  // For the implementation to be in compliance, the message below
  // should be sent whenever the data is empty, even if the
  // attribute exists.

  if (rawResponse.data.length === 0) {
    return Promise.reject(createError(404, 'Attr not found', { response: { status: 404, statusText: 'Attr not found', data: { error: 'No data for the given attribute could be found' } } }));
  }

  let respDataAux = {};
  let respData = {};

  if (r.isAllAttrs) {
    respData = parseAllAttr(deviceId, rawResponse.data);
  } else {
    respDataAux = parseOneAttr(deviceId, attr, rawResponse.data);
    if (r.isMultipleAttr) {
      respData[r.attr] = respDataAux;
    } else {
      respData = respDataAux;
    }
  }

  const newr = { ...r, respData };
  return newr;
};

module.exports = { handle };
