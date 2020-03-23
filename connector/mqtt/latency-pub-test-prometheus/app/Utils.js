const extractPayload = (message) => {
  const messageContent = JSON.parse(message);
  const { attrs } = messageContent;

  return attrs;
};

const killApplication = () => {
  process.exit(1);
};

module.exports = {
  extractPayload,
  killApplication,
};
