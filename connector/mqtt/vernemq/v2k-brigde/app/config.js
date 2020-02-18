const app = {
  mqttLogLevel: process.env.LOG_LEVEL || 'info',
};

const mqtt = {
  clientUsername: process.env.USERNAME || 'v2k-bridge-verne',
  clientId: process.env.CLIENT_ID || 'v2k-bridge-verne',
  hostname: process.env.HOSTNAME || 'v2k-bridge-verne',
  host: process.env.DOJOT_MQTT_HOST || 'vernemq-k8s',
  port: parseInt(process.env.DOJOT_MQTT_PORT, 0) || 8883,
  keepalive: 60,
  // eslint-disable-next-line no-useless-escape
  subscribeTopic: '\$share/group/+/attrs',
  subscribeQos: 1,
};

module.exports = { app, mqtt };
