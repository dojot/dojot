const app = {
  mqtt_log_level: process.env.LOG_LEVEL || 'info',
};

const mqtt = {
  mqttHost: process.env.HOSTNAME || 'v2k-bridge',
  host: process.env.DOJOT_MQTT_HOST || 'vernemq-k8s',
  port: parseInt(process.env.DOJOT_MQTT_PORT, 0) || 1883,
  keepalive: 60,
  // eslint-disable-next-line no-useless-escape
  subscribeTopic: '\$share/group/+/attrs',
  subscribeQos: 0,
};

module.exports = { app, mqtt };
