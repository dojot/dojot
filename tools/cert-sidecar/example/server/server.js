/* eslint-disable no-console */
const https = require('https');
const fs = require('fs');

try {
  const options = {
    cert: fs.readFileSync('/certs/cert.pem'),
    key: fs.readFileSync('/certs/key.pem'),
    ca: [fs.readFileSync('/certs/ca.pem')],
    rejectUnauthorized: true,
    requestCert: true,
  };

  https.createServer(options, (req, res) => {
    res.writeHead(200);
    console.log('Sending msg for client...');
    res.end('Msg from server\n');
  })
    .on('clientError', (err, socket) => {
      socket.end('HTTP/1.1 400 Bad Request\r\n\r\n');
    })
    .listen(8888);

  console.log('Server listen in 8888');
} catch (e) {
  console.error(e);
  process.kill(process.pid, 'SIGTERM');
}
