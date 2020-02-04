const { Messenger } = require('@dojot/dojot-module');
const { logger } = require('@dojot/dojot-module-logger');
const util = require('util');

const config = require('./Config');
const metrics = require('./Metrics');
const {
  convertSecToMs,
  extractPayload,
} = require('./Utils');

const TAG = { filename: 'KafkaMessenger' };

class KafkaMessenger {
  constructor() {
    logger.debug('Starting Messenger - Constructor ...', TAG);
    this.messenger = null;
  }

  init() {
    logger.info('Init Messenger ', TAG);
    logger.debug(`Init config messenger${util.inspect(config.messenger, { depth: null })}`, TAG);
    this.messenger = new Messenger('dojot_prom', config.messenger);
    this.messenger.init().then(() => {
      this.initKafka();
    }).catch((error) => {
      logger.debug(`... failed to initialize the latency-pub-test-prometheus messenger. Error: ${error.toString()}`, TAG);
    });
  }

  initKafka() {
    logger.info(`CreateChannel ${config.messenger.dojot.subjects.deviceData}`, TAG);
    this.messenger.createChannel(config.messenger.dojot.subjects.deviceData, 'r');
    const kafkaOnMessageBind = KafkaMessenger.kafkaOnMessage.bind(this);
    this.messenger.on(config.messenger.dojot.subjects.deviceData, 'message', kafkaOnMessageBind);
  }


  static kafkaOnMessage(_tenant, message, extraInfo) {
    logger.debug(`The message is ${util.inspect(message, { depth: null })}`, TAG);
    logger.debug(`The extra info is ${util.inspect(extraInfo, { depth: null })}`, TAG);

    try {
      // timestamp in ms from kafka (when msg arrives in kafka)
      const { timestamp: endTimeMS } = extraInfo;

      // payload msg mqtt, within the start timestamp in sec
      const payload = extractPayload(message);
      const { timestamp: startTimeSec } = payload;

      if (startTimeSec && endTimeMS) {
        const startTimeMs = convertSecToMs(startTimeSec);
        const totalTime = Number(endTimeMS) - startTimeMs;
        metrics.addTime(totalTime);
      }
    } catch (error) {
      logger.error(`Error parsing Kafka message: ${error}`, TAG);
    }
  }
}

module.exports = KafkaMessenger;
