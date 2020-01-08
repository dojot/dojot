/* SOAP client config for wsdl */
const soap = {
  wsdlAddr: process.env.EJBCA_WSDL_ADDR || 'https://localhost:8443/ejbca/ejbcaws/ejbcaws?wsdl',
  caCrt: process.env.EJBCA_CA_CRT_DIR || '/opt/p12/ca.crt',
  clientP12: process.env.EJBCA_VA_P12_DIR || '/opt/p12/soap_client.p12',
  clientPass: process.env.EJBCA_PASS || 'secret',
};

/* setting config for dojot messenger module */
const messenger = {
  auth: { /* auth service configuration */
    connectionRetries: 5,
    timeoutSleep: 5,
    url: process.env.AUTH_URL || 'http://auth:5000',
  },
  databroker: { /* databroker service configuration */
    connectionRetries: process.env.DATA_BROKER_CONN_RETRIES || 10,
    timeoutSleep: 2,
    url: process.env.DATA_BROKER_URL || 'http://data-broker',
  },
  dojot: { /* dojot internal events/management configuration */
    events: {
      tenantActionType: {
        CREATE: 'create',
        DELETE: 'delete',
      },
      tenantEvent: {
        DELETE_TENANT: 'delete-tenant',
        NEW_TENANT: 'new-tenant',
      },
    },
    management: {
      tenant: process.env.DOJOT_MANAGEMENT_USER || 'dojot-management',
      user: process.env.DOJOT_MANAGEMENT_USER || 'dojot-management',
    },
    subjects: {
      deviceData: process.env.DOJOT_SUBJECT_DEVICE_DATA || 'device-data',
      devices: process.env.DOJOT_SUBJECT_DEVICES || 'dojot.device-manager.device',
      tenancy: process.env.DOJOT_SUBJECT_TENANCY || 'dojot.tenancy',
    },
  },
  kafka: { /* kafka consumer/producer configuration */
    consumer: {
      'group.id': process.env.KAFKA_GROUP_ID || 'ejbca-group',
      'metadata.broker.list': process.env.KAFKA_HOSTS || 'kafka-server:9092',
    },
    dojot: {
      connectionRetries: 10,
      subjects: {
        ejbca: 'ejbca-channel',
      },
      timeoutSleep: 2,
    },
    producer: {
      dr_cb: true,
      'metadata.broker.list': process.env.KAFKA_HOSTS || 'kafka-server:9092',
      'socket.keepalive.enable': true,
    },
  },
};


/* setting config to ejbca */
const ejbcaConf = {
  ejbcaPort: process.env.EJBCA_PORT || 5583,
  apiURL: process.env.EJBCA_API_URL || 'https://dojot.github.io/ejbca-rest/apiary_latest.html',
};

module.exports = {
  soap,
  ejbcaConf,
  messenger,
};
