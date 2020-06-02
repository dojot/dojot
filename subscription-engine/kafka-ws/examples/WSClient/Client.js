// Example WS client

/* eslint-disable */
const WebSocket = require('ws');

var args = process.argv.slice(2);

const host = process.env.WS_HOST || 'localhost';
const port = parseInt(process.env.WS_PORT, 10) || 5000;

let url = `http://${host}:${port}/v1/websocket/`;

switch (args[0]) {
  case '0':
    url = url + 'ws.example.test?fields=sensor/status,temperature&where=sensor.status=in:failed,stopped;';
    break;
  case '1':
    url = url + 'ws2.example.test?fields=location&where=temperature=gte:20;';
    break;
  default:
    url = url + 'ws.example.test';
}

const ws = new WebSocket(url);

// Other test websockets with different conditions:
// const ws = new WebSocket('http://localhost:8080/v1/websocket/kafka_topic?fields=temperature,a/*/e,foo&where=foo=nin:"a,bc",d,"\\\\,\\n\\"a","\\"";');
// const ws = new WebSocket('http://localhost:8080/v1/websocket/kafka_topic?fields=temperature,a/*/e&where=foo=in:"a,bc\\n",ab\\n,"\\\\,\\",a";');
// const ws = new WebSocket('http://localhost:8080/v1/websocket/kafka_topic?fields=temperature,a/*/e&where=data.temperature=lt:30.0;rain=10;bar=neq:20;');
// const ws = new WebSocket('http://localhost:8080/v1/websocket/kafka_topic?fields=temperature,a/*/e&where=temperature=31.0;a.c.e=nin:a,1,h,2,3;');

ws.on('open', () => {
  console.log('Connected to the server.');
});

ws.on('message', (data) => {
  console.log(`Received message: ${data}`);
});

ws.on('close', (code, reason) => {
  console.log(`Connection closed.\nCode: ${code}\nReason: ${reason}`);
});

ws.on('error', (err) => {
  console.log(err);
});
