const config = {
  port: process.env.PORT || 3000,
  retrieverUrl: process.env.RETRIEVER_URL || 'http://localhost:8000',
  historyUrl: process.env.HISTORY_URL || 'http://localhost:3000',
};

module.exports = config;
