const moment = require('moment');

const generatePayload = (topic, payload) => {
  const username = topic.split('/')[0];
  const splitUsername = username.split(':');

  const tenantValue = splitUsername[0];
  const deviceIdValue = splitUsername[1];

  return {
    metadata: {
      deviceid: deviceIdValue,
      tenant: tenantValue,
      timestamp: moment().unix(),
    },
    attrs: payload,
  };
};

module.exports = { generatePayload };
