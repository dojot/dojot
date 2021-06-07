/* eslint-disable no-console */
// Example WS client
const fs = require('fs');
const WebSocket = require('ws');
const superagent = require('superagent');
const { promisify } = require('util');
const querystring = require('querystring');

const parseBoolean = (mode) => ((mode || false) && (mode.toString().toLowerCase().trim() === 'true' || Number(mode) > 0));

// Parsing environment variables
const dojotAddress = process.env.KAFKAWS_DOJOT_ADDRESS || 'localhost:8000';
const dojotUser = process.env.KAFKAWS_DOJOT_USER || 'admin';
const dojotPassword = process.env.KAFKAWS_DOJOT_PASSWORD || 'admin';
const dojotTenant = process.env.KAFKAWS_DOJOT_TENANT || 'admin';

const tls = parseBoolean(process.env.KAFKAWS_TLS_ENABLE || false);
const caFile = process.env.KAFKAWS_TLS_CA_FILE || './certs/ca.crt';

const kafkaTopic = process.env.KAFKAWS_APP_KAFKA_TOPIC || 'admin.device-data';
const filterFields = process.env.KAFKAWS_APP_FILTER_FIELDS;
const filterWhere = process.env.KAFKAWS_APP_FILTER_WHERE;

// Reading the certificate files
const ca = (tls) ? [fs.readFileSync(caFile)] : null;

async function generateAccessToken() {
  const authAddress = `${dojotAddress}/auth/realms/${dojotTenant}/protocol/openid-connect/token`;
  const payload = querystring.stringify({
    grant_type: 'password',
    client_id: 'dev-test-cli',
    username: dojotUser,
    password: dojotPassword,
  });

  console.info('Requesting a JWT to Dojot...');
  return new Promise((resolve, reject) => {
    superagent
      .post(authAddress)
      .send(payload)
      .then((res) => {
        console.info('Successfully received JWT from Dojot');
        resolve(res.body.access_token);
      })
      .catch((error) => {
        console.error(error.stack || error);
        reject(error);
      });
  });
}

async function requestTicket(accessToken) {
  console.info('Requesting a ticket needed to open a Websocket communication...');

  const protocol = (tls) ? 'https' : 'http';
  const uri = `${protocol}://${dojotAddress}/kafka-ws/v1/ticket`;
  const agent = (tls)
    ? superagent.get(uri).ca(ca)
    : superagent.get(uri);
  agent.set('Authorization', `Bearer ${accessToken}`);

  const request = promisify(agent.end).bind(agent);

  try {
    const response = await request();
    const { ticket } = response.body;
    console.info(`Obtained ticket: ${ticket}`);
    return ticket;
  } catch (error) {
    console.error(error.stack || error);
    throw error;
  }
}

function generateWebsocketURI(ticket) {
  if (!ticket) {
    console.error('No ticket has been received');
    process.exit(1);
  }

  console.info('Generating the websocket connection URI...');
  const protocol = (tls) ? 'wss' : 'ws';
  const endpoint = `${protocol}://${dojotAddress}/kafka-ws/v1/topics`;

  const queryParams = [];
  if (filterFields) {
    queryParams.push(filterFields);
  }
  if (filterWhere) {
    queryParams.push(filterWhere);
  }
  queryParams.push(`ticket=${ticket}`);

  const uri = `${endpoint}/${kafkaTopic}?${queryParams.join('&')}`;
  console.info(`Websocket connection URI generated: ${uri}`);
  return uri;
}

function startWebsocket(uri) {
  console.info('Establishing communication with the server via websocket ...');

  const ws = (tls)
    ? new WebSocket(uri, {
      ca,
      rejectUnauthorized: true,
      requestCert: true,
    })
    : new WebSocket(uri);

  ws.on('open', () => {
    console.info('Websocket on "open": Connected to the server.');
  });

  ws.on('message', (data) => {
    console.info(`Websocket on "message": Received message: ${data}`);
  });

  ws.on('close', (code, reason) => {
    console.info(`Websocket on "close": Connection closed.\nCode: ${code}\nReason: ${reason}`);
    console.info('Terminating the app, goodbye!');
    process.kill(process.pid);
  });

  ws.on('error', (err) => {
    console.info('Websocket on "error"');
    console.error(err);
  });
}

async function main() {
  console.info('Starting the client websocket application...');
  const token = await generateAccessToken();
  const ticket = await requestTicket(token);
  const wsURI = generateWebsocketURI(ticket);
  startWebsocket(wsURI);
}

// Initializing the service...
(async () => {
  try {
    console.info('Initializing...');
    await main();
  } catch (err) {
    console.error('Service will be closed', err);
    process.kill(process.pid, 'SIGTERM');
  }
})();
