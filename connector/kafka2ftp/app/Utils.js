/**
 * Gets file name, encoding and content of data received via kafka
 * @param {*} data
 */
const extractPayloadFileInfo = (data) => {
  const { value: payloadEncoded } = data;
  const payload = JSON.parse(payloadEncoded.toString());
  const { data: { filename, encoding, content } } = payload;

  return { filename, encoding, content };
};

const sleep = (ms) => new Promise((resolve) => {
  setTimeout(resolve, ms);
});

module.exports = {
  sleep,
  extractPayloadFileInfo,
};
