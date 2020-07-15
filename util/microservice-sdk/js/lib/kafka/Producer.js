const Kafka = require('node-rdkafka');
const { Logger } = require('../logging/Logger');

/**
 * Logger instance
 */
const logger = new Logger('microservice-sdk:producer');


/**
 * Timeout in ms to flush the librdkafka internal queue, sending all messages before disconnecting.
 *
 * Property: producer.flush.timeout.ms
 */
const PRODUCER_FLUSH_TIMEOUT_MS = 2000;

/**
 * Polls the producer on this interval, handling disconnections and reconnection.
 * Set it to 0 to turn it off.
 *
 * Property: producer.pool.interval.ms
 */
const PRODUCER_POLL_INTERVAL_MS = 100;

/**
 * Timeout in ms to connect
 *
 * Property: producer.connect.timeout.ms
 */
const PRODUCER_CONNECT_TIMEOUT_MS = 5000;

/**
 * Timeout in ms to disconnect
 *
 * Property: producer.disconnect.timeout.ms
 */
const PRODUCER_DISCONNECT_TIMEOUT_MS = 10000;

/**
 * Class wrapping a Kafka.KafkaProducer object.
 */
class Producer {
  /**
  * Instantiates a new Producer.
  *
  * @param {config} config the consumer Producer.
  * It is an object with the following properties:
  * - "producer.flush.timeout.ms": Timeout in ms to flush the librdkafka internal queue,
  *    sending all messages
  * - "producer.pool.interval.ms": Polls the producer on this interval,
  *    handling disconnections and reconnection. Set it to 0 to turn it off.
  * - "producer.connect.timeout.ms":  Timeout  in ms  to connect
  * - "producer.disconnect.timeout.ms":  Timeout  in ms  to disconnect
  * - "kafka.producer": an object with global properties for the node-rdkafka producer. For a full
  *    list of the properties, see:
  *    https://github.com/edenhill/librdkafka/blob/master/CONFIGURATION.md#global-configuration-properties.
  * - "kafka.topic": an object with specific topic configuration properties that applies to all
  *   topics the node-rdkafka producer produces to. For a full list of properties, see:
  *   https://github.com/edenhill/librdkafka/blob/master/CONFIGURATION.md#topic-configuration-properties.
  *
  */
  constructor(config) {
    // configuration
    this.config = config || {};

    // kafka producer configuration
    this.config['kafka.producer'] = this.config['kafka.producer'] || {};

    // kafka topic configuration
    this.config['kafka.topic'] = this.config['kafka.topic'] || {};

    this.config['producer.flush.timeout.ms'] = (
      this.config['producer.flush.timeout.ms'] || PRODUCER_FLUSH_TIMEOUT_MS
    );

    this.config['producer.pool.interval.ms'] = (
      this.config['producer.pool.interval.ms'] || PRODUCER_POLL_INTERVAL_MS
    );

    this.config['producer.connect.timeout.ms'] = (
      this.config['producer.connect.timeout.ms'] || PRODUCER_CONNECT_TIMEOUT_MS
    );

    this.config['producer.disconnect.timeout.ms'] = (
      this.config['producer.disconnect.timeout.ms'] || PRODUCER_DISCONNECT_TIMEOUT_MS
    );

    logger.debug('Creating a new Kafka producer...');


    this.isReady = false;
    this.producer = new Kafka.Producer(
      this.config['kafka.producer'],
      this.config['kafka.topic'],
    );

    if (this.isDeliveryReportEnabled()) {
      // This will set a callback to be executed everytime a message is published.
      const resolveOnDeliveryReportBind = Producer.resolveOnDeliveryReport.bind(this);
      this.producer.on('delivery-report', resolveOnDeliveryReportBind);
    }

    this.producer.setPollInterval(this.config['producer.pool.interval.ms']);

    logger.debug('... a Kafka producer was successfully created.');
  }

  /**
  * Check if the delivery report is active
  *
  * To use the event 'delivery-report', you must set request.required.acks to 1 or -1 in topic
  * configuration and dr_cb (or dr_msg_cb if you want the report to contain the message payload)
  * to true in kafka options.
  *
  * @private
  */
  isDeliveryReportEnabled() {
    return (
      this.config['kafka.producer'].dr_cb
      || this.config['kafka.producer'].dr_msg_cb
    );
  }

  /**
   * Connects the Producer to a Kafka cluster.
   *
   * @returns { Promise } A promise which will be resolved if this Producer is
   * successfully connected to a Kafka broker, or reject if any errors occur.
   */
  connect() {
    logger.info('Connecting the producer...');

    return new Promise((resolve, reject) => {
      // ready
      // note: the ready function is called just once
      this.producer.on('ready', () => {
        logger.info('Producer is ready');
        this.isReady = true;
        return resolve();
      });

      // register handler for kafka events
      // error
      this.producer.on('event.error', (event) => {
        logger.warn(`Kafka event.error: ${event}`);
      });

      // connect to kafka
      this.producer.connect(undefined, (error) => {
        if (error) {
          logger.error(`Error on connect: ${error}`);
          reject(error);
        }
      });
    });
  }

  /**
   * Produce a message to a Kafka broker.
   *
   * This function will create a message to be published to a Kafka broker. If
   * message key is set, then all messages with the same key will use the same
   * partition. If the message partition is set, then it will be used when
   * producing the message to a Kafka broker.
   *
   *
   * @param { string } topic The topic to which the message will be published
   * @param { string } message The actual message
   * @param { string } key (optional) -The key to be used to selected a partition.
   * @param { number } partition (optional) - The partition to be used when publishing the
   * message.
   */
  produce(topic, message, key = null, partition = null) {
    if (this.isReady === false) {
      logger.debug('It is not yet ready to produce.');
      return Promise.reject();
    }

    return new Promise((resolve, reject) => {
      const timestamp = Date.now();

      const buffer = Buffer.from(message);

      let callback = null;
      if (this.isDeliveryReportEnabled()) {
        callback = { resolve, reject };
      }

      // The callback object passed here will be used as an opaque object, that
      // is, the delivery report function (set in the constructor) will be
      // invoked with this object - it will resolve or reject the function
      // depending on the fallout of the produce operation.
      this.producer.produce(topic, partition, buffer, key, timestamp, { callback });

      // Poll events emit for delivery reports
      this.producer.poll();

      if (!this.isDeliveryReportEnabled()) {
        resolve();
      }
    });
  }

  /**
   * Disconnect the producer from Kafka.
   *
   * This method will cleanly disconnect from Kafka. In order to do so, it will
   *
   * - Send any pending messages;
   * - Disconnect from Kafka.
   *
   * If not connected, it won't do anything.
   *
   * @returns Promise that will be resolve
   */
  disconnect() {
    logger.debug('Disconnecting from Kafka...');
    if (this.isReady === false) {
      logger.debug('... already disconnected. Aborting operation.');
      return Promise.resolve();
    }
    logger.debug('Requesting message flushing before disconnect...');

    return new Promise((resolve, reject) => {
      this.producer.flush(this.config['producer.flush.timeout.ms'], (error) => {
        if (error) {
          return reject(error);
        }
        logger.debug('... all messages were flushed.');

        const timeoutTrigger = setTimeout(() => {
          logger.error('Unable to disconnect the producer. Timed out.');
          return reject(new Error('disconnection timeout'));
        }, this.config['producer.disconnect.timeout.ms']);

        return this.producer.disconnect((err) => {
          clearTimeout(timeoutTrigger);
          if (err) {
            logger.error(`Error while disconnecting producer: ${err}`);
            return reject(err);
          }
          logger.debug('Successfully disconnected producer from Kafka.');
          this.isReady = false;
          return resolve();
        });
      });
    });
  }

  /**
   * This callback is to be used when indicating to node-rdkafka library
   * that we want delivery reports (so we can resolve or reject message
   * producing promises.)
   * @param {str} err
   * @param {object} report
   * @private
   */
  static resolveOnDeliveryReport(err, report) {
    if (report.opaque.callback) {
      if (err) {
        report.opaque.callback.reject(err);
      } else {
        report.opaque.callback.resolve(report);
      }
    }
  }
}

module.exports = Producer;
