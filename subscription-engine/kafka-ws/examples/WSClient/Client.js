/* eslint-disable no-console */
// Example WS client
const fs = require('fs');
const WebSocket = require('ws');

const getTenant = (arg) => {
  let tenant = null;
  switch (arg) {
    case '1':
      tenant = 'tenant2';
      break;
    default:
      tenant = 'tenant1';
  }
  return tenant;
};

const createURL = (arg, host, port, tls) => {
  let urlCreate = '';
  if (tls) {
    urlCreate = 'wss://';
  } else {
    urlCreate = 'ws://';
  }
  urlCreate = `${urlCreate}${host}:${port}/v1/websocket/`;
  switch (arg) {
    case '0':
      urlCreate = `${urlCreate}tenant1.ws.example.test/?fields=sensor/status,temperature&where=sensor.status=in:failed,stopped;`;
      break;
    case '1':
      urlCreate = `${urlCreate}tenant2.ws.example.test?fields=location&where=temperature=gte:20;`;
      break;
    default:
      urlCreate = `${urlCreate}tenant1.ws.example.test`;
  }
  return urlCreate;
};

const makeJwtToken = (tenant, expSeconds, user = 'test') => {
  const payload = {
    service: tenant,
    username: user,
    exp: expSeconds,
  };
  return `${Buffer.from('jwt schema').toString('base64')}.${
    Buffer.from(JSON.stringify(payload)).toString('base64')}.${
    Buffer.from('dummy signature').toString('base64')}`;
};

const parseBoolean = (mode) => ((mode || false) && (mode.toString().toLowerCase().trim() === 'true' || Number(mode) > 0));

const args = process.argv.slice(2);

const host = process.env.WS_HOST || 'localhost';
const port = parseInt(process.env.WS_PORT, 10) || 5000;

const tls = parseBoolean(process.env.WS_TLS || false);
const caFile = process.env.WS_TLS_CA_FILE || '../certs/client/ca-cert.pem';
const keyFile = process.env.WS_TLS_KEY_FILE || '../certs/client/client-key.pem';
const certFile = process.env.WS_TLS_CERT_FILE || '../certs/client/client-cert.pem';


const url = createURL(args[0], host, port, tls);

console.info(`Trying to connect ${url}`);

const tenant = getTenant(args[0]);

const timestampInSec = Math.round(new Date().getTime() / 1000);
const tokenLiveSec = 720;
const timestampExpirationSec = timestampInSec + tokenLiveSec;

const token = makeJwtToken(tenant, timestampExpirationSec, 'user');

let ws = null;
if (tls) {
  ws = new WebSocket(url, {
    cert: fs.readFileSync(certFile),
    key: fs.readFileSync(keyFile),
    ca: [fs.readFileSync(caFile)],
    rejectUnauthorized: true,
    requestCert: true,
    headers: {
      Authorization: `Bearer ${token}`,
    },
  });
} else {
  ws = new WebSocket(url, {
    headers: {
      Authorization: `Bearer ${token}`,
    },
  });
}

// Other test websockets with different conditions:
// const ws = new WebSocket('http://localhost:5000/v1/websocket/kafka_topic?fields=temperature,a/*/e,foo&where=foo=nin:"a,bc",d,"\\\\,\\n\\"a","\\"";');
// const ws = new WebSocket('http://localhost:5000/v1/websocket/kafka_topic?fields=temperature,a/*/e&where=foo=in:"a,bc\\n",ab\\n,"\\\\,\\",a";');
// const ws = new WebSocket('http://localhost:5000/v1/websocket/kafka_topic?fields=temperature,a/*/e&where=data.temperature=lt:30.0;rain=10;bar=neq:20;');
// const ws = new WebSocket('http://localhost:5000/v1/websocket/kafka_topic?fields=temperature,a/*/e&where=temperature=31.0;a.c.e=nin:a,1,h,2,3;');

ws.on('open', () => {
  console.info('Connected to the server.');
});

ws.on('message', (data) => {
  console.log(`Received message: ${data}`);
});

ws.on('close', (code, reason) => {
  console.info(`Connection closed.\nCode: ${code}\nReason: ${reason}`);
});

ws.on('error', (err) => {
  console.error(err);
});
