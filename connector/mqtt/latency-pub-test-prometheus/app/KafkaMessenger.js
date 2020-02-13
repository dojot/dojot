const { Messenger } = require('@dojot/dojot-module');
const { logger } = require('@dojot/dojot-module-logger');

const util = require('util');
const config = require('./Config');
const latencyStore = require('./LatencyStore');

const {
  extractPayload,
  killApplication,
} = require('./Utils');

const TAG = { filename: 'KafkaMessenger' };

/**
 * Sending and receiving messages via Kafka in order to communicate with others dojot services
 */
class KafkaMessenger {
  /**
   *
   */
  constructor() {
    logger.debug('Starting Messenger - Constructor ...', TAG);
    this.messenger = null;
  }

  /**
   * Initializes the messenger
   */
  init() {
    logger.info('Init Kafka Messenger... ', TAG);
    logger.debug(`Init config messenger${util.inspect(config.messenger, { depth: null })}`, TAG);

    this.messenger = new Messenger('dojot_prom', config.messenger);

    this.messenger.init().then(() => {
      this.initKafka();
    }).catch((error) => {
      logger.error(`... failed to initialize the latency-pub-test-prometheus messenger. Error: ${error}`);
      killApplication();
    });
  }

  /**
   * Creating a new channel and process incoming data
   */
  initKafka() {
    logger.info(`CreateChannel ${config.messenger.dojot.subjects.deviceData}`, TAG);

    // Creating a new channel will inform the library that the service is interested
    // in read messages to a subject.
    this.messenger.createChannel(config.messenger.dojot.subjects.deviceData, 'r');

    // Register callback to process incoming data
    const kafkaOnMessageBind = KafkaMessenger.kafkaOnMessage.bind(this);
    this.messenger.on(config.messenger.dojot.subjects.deviceData, 'message', kafkaOnMessageBind);
  }

  /**
   * Callback to process incoming device data
   * @param {String} The tenant associated to the emitted message
   * @param {Object} Message The message (or object)
   * @param {Object} extraInfo Informations from kafka: key, topic, offset, partition and timestamp
   */
  static kafkaOnMessage(_tenant, message, extraInfo) {
    logger.debug(`The message is ${util.inspect(message, { depth: null })}`, TAG);
    logger.debug(`The extra info is ${util.inspect(extraInfo, { depth: null })}`, TAG);

    try {
      // timestamp in ms from kafka (when msg arrives in kafka)
      const { timestamp: endTimeMS } = extraInfo;

      // payload msg mqtt, within the start timestamp in ms
      const payload = extractPayload(message);
      const { timestamp: startTimeMs } = payload;

      if (startTimeMs && endTimeMS) {
        const totalTime = Number(endTimeMS) - Number(startTimeMs);

        latencyStore.addLatency(totalTime);
      }
    } catch (error) {
      logger.error(`Error parsing Kafka message: ${error}`, TAG);
    }
  }
}

module.exports = KafkaMessenger;
