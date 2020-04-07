const Kafka = require('node-rdkafka');
const { logger } = require('@dojot/dojot-module-logger');

const TAG = { filename: 'producer' };

/**
 * Class wrapping a Kafka.KafkaProducer object.
 */
class Producer {
  /**
    * Builds a new Producer.
    *
    * It is important to realize that the `config`
    * configuration are directly passed to node-rdkafka library (which will
    * forward it to librdkafka). You should check [its
    * documentation](https://github.com/edenhill/librdkafka/blob/0.11.1.x/CONFIGURATION.md)
    * to know which are all the possible settings it offers.
    * @param {config} config the configuration to be used by this object
    */
  constructor(config) {
    logger.debug('Creating a new Kafka producer...', TAG);
    this.isReady = false;
    this.producer = new Kafka.Producer(config);

    // This will set a callback to be executed everytime a message is published.
    const resolveOnDeliveryReportBind = Producer.resolveOnDeliveryReport.bind(this);
    this.producer.on('delivery-report', resolveOnDeliveryReportBind);
    this.producer.setPollInterval(100);
    logger.debug('... a Kafka producer was successfully created.', TAG);
  }

  /**
   * Connects the Producer to a Kafka cluster.
   *
   * This function will wait for 5 seconds in order to successfully connect to
   * a broker instance. If there is any problem, then it will time out or
   * node-rdkakfa will emit a event.error event, both of them triggering a
   * rejection of the returned promise.
   *
   * @returns { Promise } A promise which will be resolved if this Producer is
   * successfully connected to a Kafka broker, or reject if any errors occur.
   */
  connect() {
    logger.info('Connecting the producer...', TAG);
    const readyPromise = new Promise((resolve, reject) => {
      const timeoutTrigger = setTimeout(() => {
        logger.error('Failed to connect the producer.', TAG);
        reject(new Error('timed out'));
      }, 5000);

      this.producer.on('ready', () => {
        logger.debug('Producer is ready.', TAG);
        clearTimeout(timeoutTrigger);
        this.isReady = true;
        return resolve();
      });

      this.producer.on('event.error', (error) => {
        logger.debug(`Error while creating producer: ${error}`, TAG);
        return reject(error);
      });
    });

    this.producer.connect(null);
    return readyPromise;
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
   * @param { number } partition (optional) - The particion to be used when publishing the
   * message.
   */
  produce(topic, message, key = null, partition = null) {
    return new Promise((resolve, reject) => {
      const timeStamp = Date.now();

      const buffer = Buffer.from(message);

      const callback = { resolve, reject };

      // The callback object passed here will be used as an opaque object, that
      // is, the delivery report function (set in the constructor) will be
      // invoked with this object - it will resolve or reject the function
      // depending on the fallout of the produce operation.
      this.producer.produce(topic, partition, buffer, key, timeStamp, { callback });

      // Poll events emit for delivery reports
      this.producer.poll();
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
    logger.debug('Disconnecting from Kafka...', TAG);
    if (this.isReady === false) {
      logger.debug('... already disconnected. Aborting operation.', TAG);
      return Promise.resolve();
    }
    logger.debug('Requesting message flushing before disconnect...', TAG);
    return new Promise((resolve, reject) => {
      this.producer.flush(2000, (error) => {
        if (error) {
          return reject(error);
        }
        logger.debug('... all messages were flushed.', TAG);

        const timeoutTrigger = setTimeout(() => {
          logger.error('Unable to disconnect the producer. Timed out.', TAG);
          return reject(new Error('disconnection timeout'));
        }, 10000);

        return this.producer.disconnect((err) => {
          clearTimeout(timeoutTrigger);
          if (err) {
            logger.error(`Error while disconnecting producer: ${err}`);
            return reject(err);
          }
          logger.debug('Successfully disconnected producer from Kafka.', TAG);
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
