/* eslint-disable class-methods-use-this */
const listeners = new Map();

const sendFakePayload = (topic, message, ack) => {
  const payload = {
    value: Buffer.from(message),
  };

  const cb = listeners.get(topic);
  cb(payload, ack);
};

class ConsumerTest {
  constructor(config) {
    this.config = config;
  }

  init() {
    this.listenersIndex = [];
  }

  registerCallback(topic, callback) {
    listeners.set(topic.source, callback);
    this.listenersIndex.push(topic.source);

    return this.listenersIndex.length - 1;
  }

  unregisterCallback(registerId) {
    // Only test
    // eslint-disable-next-line security/detect-object-injection
    const topic = this.listenersIndex[registerId];
    listeners.delete(topic.source);
  }

  getStatus() {
    return !!listeners;
  }
}

module.exports = {
  ConsumerTest,
  sendFakePayload,
};
