// Example WS client

/* eslint-disable */
const WebSocket = require('ws');

const ws = new WebSocket('http://localhost:8080/v1/websocket/kafka_topic?fields=temperature,a/*/e,foo&where=foo=nin:"a,bc",d,"\\\\,\\n\\"a","\\"";');
// Other test websockets with different conditions:
// const ws = new WebSocket('http://localhost:8080/v1/websocket/kafka_topic?fields=temperature,a/*/e&where=foo=in:"a,bc\\n",ab\\n,"\\\\,\\",a";');
// const ws = new WebSocket('http://localhost:8080/v1/websocket/kafka_topic?fields=temperature,a/*/e&where=data.temperature=lt:30.0;rain=10;bar=neq:20;');
// const ws = new WebSocket('http://localhost:8080/v1/websocket/kafka_topic?fields=temperature,a/*/e&where=temperature=31.0;a.c.e=nin:a,1,h,2,3;');

// let interval;

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
