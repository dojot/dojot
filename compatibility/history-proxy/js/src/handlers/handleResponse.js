const { Logger } = require('@dojot/microservice-sdk');

const createError = require('http-errors');

const logger = new Logger('history-proxy:express/handle/handle-response');

/**
  * Parsing data when was requested only one attribute
  *
  * @param {string} deviceId
  * @param {string} attribute_label
  * @param {json} data
  */
const parseOneAttr = (deviceId, attr, data) => data.map((element) => {
  const newEl = { device_id: deviceId };
  newEl.ts = element.ts;
  newEl.value = element.value;
  newEl.attr = attr;
  newEl.metadata = {};
  return newEl;
});

/**
  * Parsing data when was requested all device attributes
  *
  * @param {string} deviceId
  * @param {json} data
  */
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

      // To avoid security issues in dynamic object-injection
      if (Object.prototype.hasOwnProperty.call(objRtn, label)) {
        const { [label]: valueLabel } = objRtn;
        Object.defineProperty(objRtn, label, {
          value: [...valueLabel, newEl],
          writable: true,
          enumerable: true
        });
      } else {
        Object.defineProperty(objRtn, label, {
          value: [newEl],
          writable: true,
          enumerable: true
        });
      }
    });
  });
  return objRtn;
};

/**
  * Parsing the data received from Influx
  *
  * @param {object} pipelineData
  *
  * @return {object} pipelineData
  */
const handle = (r) => {
  const { rawResponse, deviceId, attr } = r;
  logger.debug(`Received data: ${JSON.stringify(rawResponse)}`);

  // For the implementation to be in compliance, the message below
  // should be sent whenever the data is empty, even if the
  // attribute exists.

  if (rawResponse.length === 0) {
    return Promise.reject(createError(404, 'Attr not found', { response: { status: 404, statusText: 'Attr not found', data: { error: 'No data for the given attribute could be found' } } }));
  }

  let respDataAux = {};
  let respData = {};

  if (r.isAllAttrs) {
    respData = parseAllAttr(deviceId, rawResponse);
  } else {
    respDataAux = parseOneAttr(deviceId, attr, rawResponse);
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
