const app = {
  log_level: process.env.LOG_LEVEL || 'info',
};

const messenger = {
  kafka: {
    producer: {
      'metadata.broker.list': process.env.KAFKA_HOSTS || 'kafka-server:9092',
      'compression.codec': 'gzip',
      'retry.backoff.ms': 200,
      'message.send.max.retries': 10,
      'socket.keepalive.enable': true,
      'queue.buffering.max.messages': 100000,
      'queue.buffering.max.ms': 1000,
      'batch.num.messages': 1000000,
      dr_cb: true,
    },
    consumer: {
      'group.id': 'prom-group',
      'metadata.broker.list': process.env.KAFKA_HOSTS || 'kafka-server:9092',
    },
    dojot: {
      subscriptionHoldoff: Number(process.env.DOJOT_SUBSCRIPTION_HOLDOFF) || 2500,
      timeoutSleep: 5,
      connectionRetries: 5,
    },
  },
  databroker: {
    url: process.env.DATA_BROKER_URL || 'http://data-broker',
    timeoutSleep: 2,
    connectionRetries: 5,
  },
  auth: {
    url: process.env.AUTH_URL || 'http://auth:5000',
    timeoutSleep: 5,
    connectionRetries: 5,
  },
  deviceManager: {
    url: process.env.DEVICE_MANAGER_URL || 'http://device-manager:5000',
    timeoutSleep: 5,
    connectionRetries: 3,
  },
  dojot: {
    management: {
      user: process.env.DOJOT_MANAGEMENT_USER || 'dojot-management',
      tenant: process.env.DOJOT_MANAGEMENT_TENANT || 'dojot-management',
    },
    subjects: {
      tenancy: process.env.DOJOT_SUBJECT_TENANCY || 'dojot.tenancy',
      devices: process.env.DOJOT_SUBJECT_DEVICES || 'dojot.device-manager.device',
      deviceData: process.env.DOJOT_SUBJECT_DEVICE_DATA || 'device-data',
    },
    events: {
      tenantEvent: {
        NEW_TENANT: 'new-tenant',
        DELETE_TENANT: 'delete-tenant',
      },
      tenantActionType: {
        CREATE: 'create',
        DELETE: 'delete',
      },
    },
  },
};

const prom = {
  port: Number(process.env.PROMETHEUS_PORT) || 3000,
};

module.exports = {
  app,
  messenger,
  prom,
};
