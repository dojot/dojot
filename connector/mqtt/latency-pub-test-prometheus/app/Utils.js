const extractPayload = (message) => {
  const messageContent = JSON.parse(message);
  const { attrs } = messageContent;

  return attrs;
};

const killApplication = () => {
  process.kill(process.pid, 'SIGTERM');
};

module.exports = {
  extractPayload,
  killApplication,
};
