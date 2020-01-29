
const convertSecToMs = (startTimeSec) => parseInt(Math.floor(startTimeSec * 1000), 10);

const extractPayload = (message) => {
  const messageContent = JSON.parse(message);
  const { attrs } = messageContent;

  return attrs;
};


module.exports = {
  convertSecToMs,
  extractPayload,
};
