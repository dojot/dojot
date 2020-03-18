const hostname = process.env.HOSTNAME || 'k2v-bridge';
const parseSecureMode = (mode) => ((mode || false) && (mode.toString().toLowerCase().trim() === 'true' || Number(mode) > 0));

const app = {
  logLevel: process.env.K2V_LOG_LEVEL || 'info',
  baseDir: process.env.BASE_DIR || '/opt/k2v-bridge',
  hostname,
};

const mqtt = {
  clientUsername: process.env.K2V_MQTT_USERNAME || 'k2v-bridge',
  clientId: process.env.K2V_MQTT_CLIENT_ID || hostname,
  host: process.env.K2V_MQTT_HOST || 'vernemq-k8s',
  port: parseInt(process.env.K2V_MQTT_PORT, 10) || 8883,
  keepAlive: parseInt(process.env.K2V_MQTT_KEEPALIVE, 10) || 60,
  secure: parseSecureMode(process.env.K2V_MQTT_SECURE || true),
  publishTopicSuffix: process.env.K2V_MQTT_PUBLISH_TOPIC_SUFFIX || '/config',
  publishQos: parseInt(process.env.K2V_MQTT_PUBLISH_QOS, 10) || 1,
  tls: {
    ca: {
      location: process.env.K2V_MQTT_CA_FILE || `${app.baseDir}/app/cert/ca.crt`,
    },
    certificate: {
      location: process.env.K2V_MQTT_CERT_FILE || `${app.baseDir}/app/cert/${hostname}.crt`,
    },
    privateKey: {
      location: process.env.K2V_MQTT_KEY_FILE || `${app.baseDir}/app/cert/${hostname}.key`,
    },
  },
};

module.exports = { app, mqtt, parseSecureMode };
