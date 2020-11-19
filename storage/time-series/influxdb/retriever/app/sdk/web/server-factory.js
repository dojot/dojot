const http = require('http');
const https = require('https');
const fs = require('fs');

const containsAll = (source, target) => target.every((el) => source.includes(el));

function createObject(config, logger) {
  let server = null;
  if (containsAll(Object.keys(config), ['cert', 'key', 'ca'])) {
    const serverCfg = { ...config };
    if (!Array.isArray(serverCfg.ca)) {
      serverCfg.ca = [serverCfg.ca];
    }
    serverCfg.cert = fs.readFileSync(serverCfg.cert);
    serverCfg.key = fs.readFileSync(serverCfg.key);
    serverCfg.ca = serverCfg.ca.map((filename) => fs.readFileSync(filename));

    logger.debug('Creating the Web Server (with TLS encryption)');
    server = https.createServer(serverCfg);
  } else {
    logger.debug('Creating the Web Server');
    server = http.createServer(config);
  }
  logger.debug('Web Server created!');
  return server;
}

module.exports = ({ config, logger }) => createObject(config, logger);
