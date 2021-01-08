/* eslint-disable no-console */
const https = require('https');
const fs = require('fs');

try {
  const options = {
    host: 'server',
    path: '/',
    port: '8888',
    method: 'GET',
    cert: fs.readFileSync('/certs/cert.pem'),
    key: fs.readFileSync('/certs/key.pem'),
    ca: [fs.readFileSync('/certs/ca.pem')],
  };

  setInterval(() => {
    https.request(options, (response) => {
      response.on('data', (chunk) => {
        // eslint-disable-next-line security-node/detect-crlf
        console.log(`Response from server: ${chunk}`);
      });
    })
      .on('error', (e) => {
        console.error(`problem with request: ${e}`);
        throw e;
      })
      .end();
  }, 5000);
} catch (e) {
  // eslint-disable-next-line security-node/detect-crlf
  console.log(e);
  process.kill(process.pid, 'SIGTERM');
}
