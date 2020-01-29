const config = require('./config');
const MQTTClient = require('./MqttClient');

const client = new MQTTClient(config);
client.init();
