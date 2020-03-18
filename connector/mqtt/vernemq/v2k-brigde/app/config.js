const hostname = process.env.HOSTNAME || 'v2k-bridge';
const unsecuredMode = (mode) => ((mode || false) && (mode.toString().toLowerCase().trim() === 'true' || Number(mode) > 0));

const app = {
  logLevel: process.env.V2K_LOG_LEVEL || 'info',
  baseDir: process.env.BASE_DIR || '/opt/v2k_bridge',
  hostname,
};

const mqtt = {
  clientUsername: process.env.V2K_MQTT_USERNAME || 'v2k-bridge',
  clientId: process.env.V2K_MQTT_CLIENT_ID || hostname,
  host: process.env.V2K_MQTT_HOST || 'vernemq-k8s',
  port: parseInt(process.env.V2K_MQTT_PORT, 0) || 8883,
  keepalive: parseInt(process.env.V2K_MQTT_KEEPALIVE, 0) || 60,
  secure: unsecuredMode(process.env.V2K_MQTT_SECURE || true),
  // eslint-disable-next-line no-useless-escape
  subscribeTopic: process.env.V2K_MQTT_SUBSCRIPTION_TOPIC || '\$share/group/+/attrs',
  subscribeQos: parseInt(process.env.V2K_MQTT_SUBSCRIPTION_QOS, 0) || 1,
  parallelHandlers: parseInt(process.env.V2K_BACKPRESSURE_PARALLEL_HANDLERS, 0) || 4,
  maxQueueLength: parseInt(process.env.V2K_BACKPRESSURE_MAX_QUEUE_LENGTH, 0) || 1048576,
  tls: {
    ca: {
      location: process.env.V2K_MQTT_CA_FILE || `${app.baseDir}/app/cert/ca.crt`,
    },
    certificate: {
      location: process.env.V2K_MQTT_CERT_FILE || `${app.baseDir}/app/cert/${hostname}.crt`,
    },
    privateKey: {
      location: process.env.V2K_MQTT_KEY_FILE || `${app.baseDir}/app/cert/${hostname}.key`,
    },
  },
};

module.exports = { app, mqtt, unsecuredMode };
