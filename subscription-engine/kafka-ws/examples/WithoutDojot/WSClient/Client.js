/* eslint-disable no-console */
// Example WS client
const fs = require('fs');
const WebSocket = require('ws');
const superagent = require('superagent');
const jwt = require('jsonwebtoken');
const { promisify } = require('util');

const jwtSignAsync = promisify(jwt.sign).bind(jwt);

const parseBoolean = (mode) => ((mode || false) && (mode.toString().toLowerCase().trim() === 'true' || Number(mode) > 0));

const host = process.env.WS_HOST || 'localhost';
const port = parseInt(process.env.WS_PORT, 10) || 5000;

const tls = parseBoolean(process.env.WS_TLS || false);
const caFile = process.env.WS_TLS_CA_FILE || '../certs/client/ca-cert.pem';
const keyFile = process.env.WS_TLS_KEY_FILE || '../certs/client/client-key.pem';
const certFile = process.env.WS_TLS_CERT_FILE || '../certs/client/client-cert.pem';

const ca = (tls) ? [fs.readFileSync(caFile)] : null;
const key = (tls) ? fs.readFileSync(keyFile) : null;
const cert = (tls) ? fs.readFileSync(certFile) : null;

function getTenant(option) {
  let tenant = null;
  switch (option) {
    case '1':
      tenant = 'tenant2';
      break;
    default:
      tenant = 'tenant1';
  }
  return tenant;
}

async function generateAccessToken(tenant) {
  console.info('Generating a dummy JWT Access Token...');
  const expirationSec = 60;
  const token = await jwtSignAsync({ service: tenant }, 'a dummy secret', { expiresIn: expirationSec });
  console.info(`Generated token. It will expire in ${expirationSec} seconds`);
  return token;
}

async function requestTicket(accessToken) {
  console.info('Requesting a ticket needed to open a websocket communication...');
  const protocol = (tls) ? 'https' : 'http';
  const uri = `${protocol}://${host}:${port}/kafka-ws/v1/ticket`;
  const agent = (tls)
    ? superagent.get(uri).ca(ca).key(key).cert(cert)
    : superagent.get(uri);
  agent.set('Authorization', `Bearer ${accessToken}`);
  const request = promisify(agent.end).bind(agent);
  const response = await request();
  const { ticket } = response.body;
  console.info(`Obtained ticket: ${ticket}`);
  return ticket;
}

function generateWebsocketURI(option, ticket) {
  console.info('Generating the websocket connection URI...');
  const protocol = (tls) ? 'wss' : 'ws';
  const endpoint = `${protocol}://${host}:${port}/kafka-ws/v1/topics`;
  let resource;
  const queryParams = [];
  if (option === '0') {
    resource = 'tenant1.ws.example.test';
    queryParams.push('fields=sensor/status,temperature');
    queryParams.push('where=sensor.status=in:failed,stopped;');
  } else if (option === '1') {
    resource = 'tenant2.ws.example.test';
    queryParams.push('fields=location');
    queryParams.push('where=temperature=gte:20;');
  } else {
    resource = 'tenant1.ws.example.test';

    // Other websocket tests with different conditions:
    // queryParams.push(
    //   'fields=temperature,a/*/e,foo',
    //   'where=foo=nin:"a,bc",d,"\\\\,\\n\\"a","\\"";',
    // );
    // queryParams.push(
    //   'fields=temperature,a/*/e',
    //   'where=foo=in:"a,bc\\n",ab\\n,"\\\\,\\",a";',
    // );
    // queryParams.push(
    //   'fields=temperature,a/*/e',
    //   'where=data.temperature=lt:30.0;rain=10;bar=neq:20;',
    // );
    // queryParams.push(
    //   'fields=temperature,a/*/e',
    //   'where=temperature=31.0;a.c.e=nin:a,1,h,2,3;',
    // );
  }
  if (ticket) {
    queryParams.push(`ticket=${ticket}`);
  }
  const uri = `${endpoint}/${resource}?${queryParams.join('&')}`;
  console.info(`Websocket connection URI generated: ${uri}`);
  return uri;
}

function startWebsocket(uri) {
  console.info('Establishing communication with the server via websocket ...');

  const ws = (tls)
    ? new WebSocket(uri, {
      ca,
      key,
      cert,
      rejectUnauthorized: true,
      requestCert: true,
    })
    : new WebSocket(uri);

  ws.on('open', () => {
    console.info('Websocket on "open": Connected to the server.');
  });

  ws.on('message', (data) => {
    console.log(`Websocket on "message": Received message: ${data}`);
  });

  ws.on('close', (code, reason) => {
    console.info(`Websocket on "close": Connection closed.\nCode: ${code}\nReason: ${reason}`);
  });

  ws.on('error', (err) => {
    console.info('Websocket on "error"');
    console.error(err);
  });
}

async function main(args) {
  console.info('Starting the client websocket application...');
  const tenantOption = args[0];
  const tenant = getTenant(tenantOption);
  const token = await generateAccessToken(tenant);
  const ticket = await requestTicket(token);
  const wsURI = generateWebsocketURI(tenantOption, ticket);
  startWebsocket(wsURI);
}

// Initializing the service...
(async () => {
  try {
    console.info('Initializing...');
    await main(process.argv.slice(2));
  } catch (err) {
    console.error('Service will be closed', err);
    process.kill(process.pid, 'SIGTERM');
  }
})();
