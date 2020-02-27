const hostName = process.env.HOSTNAME || 'k2v-bridge-verne';
const toBoolean = (mode) => ((mode || false) && (mode.toString().toLowerCase().trim() === 'true' || Number(mode) > 0));

const app = {
  logLevel: process.env.LOG_LEVEL || 'info',
  baseDir: process.env.BASE_DIR || '/opt/k2v-verne',
  hostname: hostName,
};

const mqtt = {
  clientUsername: process.env.K2V_MQTT_USERNAME || 'k2v-bridge-verne',
  clientId: process.env.k2V_MQTT_CLIENT_ID || hostName,
  host: process.env.K2V_MQTT_HOST || 'vernemq-k8s',
  port: parseInt(process.env.K2V_MQTT_PORT, 0) || 8883,
  keepAlive: parseInt(process.env.K2V_MQTT_KEEPALIVE, 0) || 60,
  secure: toBoolean(process.env.K2V_MQTT_SECURE),
  publishTopicSuffix: process.env.K2V_MQTT_PUBLISH_TOPIC_SUFFIX || '/config',
  publishQos: parseInt(process.env.K2V_MQTT_SUBSCRIPTION_QOS, 0) || 1,
  tls: {
    ca: {
      location: process.env.K2V_MQTT_CA_FILE || `${app.baseDir}/app/cert/ca.crt`,
    },
    certificate: {
      location: process.env.K2V_MQTT_CERT_FILE || `${app.baseDir}/app/cert/${hostName}.crt`,
    },
    privateKey: {
      location: process.env.K2V_MQTT_KEY_FILE || `${app.baseDir}/app/cert/${hostName}.key`,
    },
  },
};

module.exports = { app, mqtt, toBoolean };
