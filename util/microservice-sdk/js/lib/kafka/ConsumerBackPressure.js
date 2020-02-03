const async = require('async');
const Kafka = require('node-rdkafka');
const uuidv4 = require('uuid/v4');
const { logger } = require('@dojot/dojot-module-logger');
const CommitManager = require('./CommitManager.js');

/**
 * The logger identifier
 */
const TAG = { filename: 'kfk-consumer' };

/**
 * Default value of parallel handlers to deal with the messages
 */
const DEFAULT_PARALLEL_HANDLERS = 1;
/**
 * Default value of bytes supported by the 'msgQueue'
 */
const DEFAULT_MAX_QUEUE_BYTES = 1048576; // 1 mb
/**
 * Default value of the hold off time for updating the subscripts (in ms)
 */
const DEFAULT_HOLD_OFF_TIME_SUBSCRIPTION = 1000;
/**
 * Default value of the maximum hold off time for updating the subscripts (in ms)
 */
const DEFAULT_MAX_HOLD_OFF_TIME_SUBSCRIPTION = 10000;
/**
 * Default value of the hold off time factor for updating the subscripts
 */
const DEFAULT_HOLD_OFF_TIME_SUBSCRIPTION_FACTOR = 1.5;

/**
 * This class is a wrapper over the node-rdkafka implementation. It adds:
 * - a register callback system for messages;
 * - a mechanism to control the processing flow that allows the consuming be
 * paused when a given watermark is reached;
 * - a commit management, ensuring that all message will be processed at least
 * once.
 * NOTE: Let's keep an healthy relationship, so please do not use the
 * class members, use the setters. Thanks :)
 */
module.exports = class ConsumerBackPressure {
  /**
   * Instantiates a new kafka consumer.
   * @param {*} consumerConfig the consumer configuration. For more details,
   * please, refer to the node-rdkafka documentation.
   */
  constructor(consumerConfig) {
    this.consumerConfig = JSON.parse(JSON.stringify(consumerConfig));
    this.consumerConfig['enable.auto.commit'] = false;
    this.consumerConfig.rebalance_cb = this.onRebalance.bind(this);

    this.isWaitingForRefreshSubscriptions = false;
    this.holdOffTimeSubscription = DEFAULT_HOLD_OFF_TIME_SUBSCRIPTION;
    this.maxHoldOffTimeSubscription = DEFAULT_MAX_HOLD_OFF_TIME_SUBSCRIPTION;
    this.holdOffFactorTimeSubscription = DEFAULT_HOLD_OFF_TIME_SUBSCRIPTION_FACTOR;
    this.topicMap = {};
    this.topicRegExpArray = [];
    this.consumer = null;
    this.msgQueue = null;
    this.isReady = false;
    this.maxParallelHandlers = DEFAULT_PARALLEL_HANDLERS;
    this.maxQueueBytes = DEFAULT_MAX_QUEUE_BYTES;
    this.currQueueBytes = 0;
    this.isPaused = false;
    this.commitManager = null;
    logger.info(`Kafka features: ${Kafka.features}`, TAG);
    logger.info(`Consumer configuration: ${JSON.stringify(this.consumerConfig)}`);
  }

  /**
   * Initializes the consumer.
   * @param {*} commitInterval the interval in ms that the message offset should be committed
   * @returns a Promise that is fullfil when the consumer becomes ready.
   */
  async init(commitInterval) {
    return new Promise((resolve) => {
      this.consumer = new Kafka.KafkaConsumer(this.consumerConfig);
      this.commitManager = new CommitManager(this.consumer.commit.bind(this.consumer),
        commitInterval);

      this.consumer.on('event.error', (event) => {
        logger.warn(`Kafka event.error: ${event}`, TAG);
      });

      // note: the ready function is called just once
      this.consumer.on('ready', () => {
        logger.info('Consumer is ready', TAG);
        this.isReady = true;
        this.commitManager.init();
        this.refreshSubscriptions(0);
        this.consumer.consume();
        return resolve();
      });

      this.consumer.on('data', this.onData.bind(this));

      // create an async queue to deal with the messages in a parallel way
      this.msgQueue = async.queue(async (data, done) => {
        await this.invokeInterestedCallbacks(data);
        done();
      }, this.maxParallelHandlers);

      // when the processing was finalized, we need to resume the consumer, if
      // it has been paused
      this.msgQueue.drain(this.resumeConsumer);

      this.consumer.connect(undefined, (error) => {
        if (error) {
          logger.error(`Error on connect: ${error}`, TAG);
        }
      });
    });
  }

  /**
   * Registers a callback to handle messages from a specific kafka topic.
   * If the kafka consumer is not subscribed on the given topic, it does.
   * Note that:
   * - subscriptions are not made immediately, it waits the 'holdOffTimeSubscription'
   * to do it;
   * - any errors are given if the topic does not exist, if in the future
   * the topic is created, the consumer will subscribe on it in
   * 'topic.metadata.refresh.interval.ms' ms (configurable by consumerConfig on
   * class constructor).
   *
   * @param {*} topic the target kafka topic, it could be a String or a RegExp
   * @param {*} callback async callback (data): Promise
   * Where data is an object with the following content:
   *   {
   *     value: Buffer.from('hi'), // message contents as a Buffer
   *     size: 2, // size of the message, in bytes
   *     topic: 'librdtesting-01', // topic the message comes from
   *     offset: 1337, // offset the message was read from
   *     partition: 1, // partition the message was on
   *     key: 'someKey', // key of the message if present
   *     timestamp: 1510325354780 // timestamp of message creation
   *   }
   * .
   *
   * @return an identifier (String) that represents the association between the topic
   * and the callback. You need to store it if you intend to remove this association
   * in some moment.
   *
   * @example subscribe(/^notifications\/.*\/critical/), handleAllCriticalNotifications)
   */
  registerCallback(topic, callback) {
    logger.debug(`subscribing on topic: ${topic}`, TAG);

    let needToRefreshSubscriptions = true;
    const id = uuidv4();
    const registryEntry = {
      callback,
      id,
    };

    if (typeof (topic) === 'object') {
      registryEntry.regExp = topic;
      this.topicRegExpArray.push(registryEntry);
    } else {
      // init structure
      this.topicMap[topic] = this.topicMap[topic] || [];
      // add new entry
      this.topicMap[topic].push(registryEntry);
      if (this.topicMap[topic].length > 1) {
        needToRefreshSubscriptions = false;
      }
    }

    // update kafka topic subscriptions
    if (needToRefreshSubscriptions) {
      this.refreshSubscriptions(this.holdOffTimeSubscription);
    }

    return id;
  }

  /**
   * Unregister a callback previously registered by the 'registerCallback' method.
   * @param {*} registerId the subscription id that belongs to the callback register
   * that you desire to remove
   */
  unregisterCallback(registerId) {
    logger.debug(`unsubscribing id: ${registerId}`, TAG);
    let needToRefreshSubscriptions = false;
    const topicsToBeRemoved = [];

    // check if the id belongs to a regular expression registry
    const topicsRegExpBefore = this.topicRegExpArray.length;
    this.topicRegExpArray = this.topicRegExpArray.filter((entry) => entry.id !== registerId);
    if (topicsRegExpBefore !== this.topicRegExpArray.length) {
      needToRefreshSubscriptions = true;
    } else {
      Object.keys(this.topicMap).forEach((topic) => {
        this.topicMap[topic] = this.topicMap[topic].filter(
          (entry) => entry.id !== registerId,
        );
        if (this.topicMap[topic].length === 0) {
          needToRefreshSubscriptions = true;
          topicsToBeRemoved.push(topic);
        }
      });

      topicsToBeRemoved.forEach((topic) => {
        delete this.topicMap[topic];
      });
    }

    if (needToRefreshSubscriptions) {
      this.refreshSubscriptions(this.holdOffTimeSubscription);
    }
  }

  /**
   * Configures the hold off time for updating the subscriptions.
   * This is the time that a call for the 'registerCallback' will wait
   * for refreshing the subscriptions with the kafka if a new topic is requested.
   * The timer is not restarted if a new call to register is performed before a
   * previous timer expires.
   * @param {*} holdOffTimeSubscription the hold off time in ms
   * @example
   * This 'holdOffTimeSubscription' is configured with 2000 ms. Someone calls
   * registerCallback('TopicOne', topicOneHandler), after 1000 ms the method
   * is called again with other topic, something like
   * registerCallback('TopicTwo', topicTwoHandler). In this scenario the subscription
   * to both the topics, 'TopicOne' and 'TopicTwo', will be performed 2000 ms
   * after the first call of the registerCallback.
   */
  setHoldOffTimeSubscription(holdOffTimeSubscription) {
    this.holdOffTimeSubscription = holdOffTimeSubscription;
  }

  /**
   * Configures the hold off time's increment factor in failure scenarios.
   * @param {*} holdOffFactorTimeSubscription a scalar that represents the hold off increment factor
   * @example
   * Given the following scenario:
   * - 'holdOffTimeSubscription' is 2000
   * - 'holdOffFactorTimeSubscription' is 2.0
   * - the regiterCallback is called at 60000 (a fictional time mark)
   * The subscription is performed at 62000, but it fails, so a new try is
   * scheduled for 66000 (62000 + 2.0 * 2000).
   */
  setHoldOffFactorTimeSubscription(holdOffFactorTimeSubscription) {
    this.holdOffFactorTimeSubscription = holdOffFactorTimeSubscription;
  }

  /**
   * Configures the maximum amount of time that the hold off time can reach in
   * failure scenarios.
   * @param {*} maxHoldOffTimeSubscription the value of the maximum hold off time in ms
   */
  setMaxHoldOffTimeSubscription(maxHoldOffTimeSubscription) {
    this.maxHoldOffTimeSubscription = maxHoldOffTimeSubscription;
  }

  /**
   * Configures the maximum of message handlers that should work in parallel
   * @param {*} maxParallelHandlers the number maximum of parallel handlers
   */
  setMaxParallelHandlers(maxParallelHandlers) {
    this.maxParallelHandlers = maxParallelHandlers;
  }

  /**
   * Configures the maximum size in bytes that the message queue can handle. If
   * this limit is exceeded the consumer is paused until all messages have been
   * processed, so the consumer is resumed.
   * @param {*} maxQueueBytes the maximum bytes that the processing queue can handle
   */
  setMaxQueueBytes(maxQueueBytes) {
    this.maxQueueBytes = maxQueueBytes;
  }

  /**
   * Refreshes the subscription on kafka.
   * @access private
   * @param {*} holdOff the hold off time to call the subscription procedure
   */
  refreshSubscriptions(holdOff) {
    // just schedule if does not have a previous timer scheduled and if
    // the consumer is ready
    if ((!this.isWaitingForRefreshSubscriptions) && (this.isReady)) {
      this.isWaitingForRefreshSubscriptions = true;

      const subscriptionProcedure = (currentHoldOff) => {
        logger.debug('Refreshing subscriptions', TAG);

        // According to the node-rdkafka documentation we need to call
        // the unsubscribe method before call the subscribe with new topics
        try {
          this.consumer.unsubscribe();
          // concatenates the topics explicits with the regular expressions
          const topics = Array.prototype.concat(Object.keys(this.topicMap),
            this.topicRegExpArray.map((entry) => entry.regExp));
          logger.debug(`subscribing in the following topics (${topics.length}): ${JSON.stringify(topics)}`);
          if (topics.length > 0) {
            this.consumer.subscribe(topics);
          }
          this.isWaitingForRefreshSubscriptions = false;
        } catch (error) {
          logger.warn(`Error while subscribing: ${error}`, TAG);
          // computes the next hold off time
          let nextHoldOffTimeCandidate = currentHoldOff * this.holdOffFactorTimeSubscription;
          nextHoldOffTimeCandidate = nextHoldOffTimeCandidate > this.maxHoldOffTimeSubscription
            ? this.maxHoldOffTimeSubscription : nextHoldOffTimeCandidate;
          nextHoldOffTimeCandidate = nextHoldOffTimeCandidate
          || this.holdOffTimeSubscription;
          // schedules the next attempt
          setTimeout(subscriptionProcedure, nextHoldOffTimeCandidate, nextHoldOffTimeCandidate);
        }
      };

      setTimeout(subscriptionProcedure, holdOff, holdOff);
    }
  }

  /**
   * Handles the kafka's rebalance event.
   * @access private
   * @param {*} error the error code
   * @param {*} assignments the assignments
   */
  async onRebalance(error, assignments) {
    if (error.code === Kafka.CODES.ERRORS.ERR__ASSIGN_PARTITIONS) {
      this.consumer.assign(assignments);
    } else if (error.code === Kafka.CODES.ERRORS.ERR__REVOKE_PARTITIONS) {
      if (this.isPaused) {
        this.consumer.resume(assignments);
        this.isPaused = false;
        logger.info('Consumer resumed', TAG);
      }
      // when partition are revoked we just abort queued tasks and do not
      // commit any task that is in processing
      this.msgQueue.remove(() => true);
      this.consumer.unassign();
      this.commitManager.onRebalance();
    } else {
      logger.warn(`Rebalance error : ${error}`, TAG);
    }
  }

  /**
   * Handles the raw messages that has been come from the Kafka
   * @access private
   * @param {*} data the received message with the following attributes:
   * {
   *   value: Buffer.from('konnichiwa'), // message contents as a Buffer
   *   size: 10, // size of the message, in bytes
   *   topic: 'greetings', // topic the message comes from
   *   offset: 1337, // offset the message was read from
   *   partition: 1, // partition the message was on
   *   key: 'someKey', // key of the message if present
   *   timestamp: 1510325354780, // timestamp of message creation
   * }
   */
  onData(data) {
    this.commitManager.notifyStartProcessing(data);
    this.currQueueBytes += data.size;
    this.msgQueue.push(data, () => {
      this.currQueueBytes -= data.size;
    });

    logger.debug(`Current queue utilization: ${this.currQueueBytes}/${this.maxQueueBytes} bytes`, TAG);

    // checks is the queue is full or not
    if (this.currQueueBytes > this.maxQueueBytes) {
      logger.info('Consumer paused due to queue capacity overflow', TAG);
      this.consumer.pause(this.consumer.assignments());
      this.isPaused = true;
    }
  }

  /**
   * Given a kafka message this method invokes all interested callbacks based on
   * the message's topic
   * @access private
   * @param {*} data the kafka message with the following attributes:
   * {
   *   value: Buffer.from('konnichiwa'), // message contents as a Buffer
   *   size: 10, // size of the message, in bytes
   *   topic: 'greetings', // topic the message comes from
   *   offset: 1337, // offset the message was read from
   *   partition: 1, // partition the message was on
   *   key: 'someKey', // key of the message if present
   *   timestamp: 1510325354780, // timestamp of message creation
   * }
   */
  async invokeInterestedCallbacks(data) {
    try {
      // verifies if the topic does not matches with a regular expression
      this.topicRegExpArray.forEach(async (entry) => {
        if (entry.regExp.test(data.topic)) {
          logger.debug(`Message on topic: ${data.topic} . Calling callback: ${entry.id}`, TAG);
          try {
            await entry.callback(data);
          } catch (error) {
            logger.warn(`Error on user's callback ${entry.id} topic: ${data.topic}: ${error}`, TAG);
          }
        }
      });
      // checks if there isn't a handler to the topic explicitly
      if (!this.topicMap[data.topic]) {
        // just skip
        return;
      }
      // iterates by the callbacks
      this.topicMap[data.topic].forEach(async (entry) => {
        logger.debug(`Message on topic: ${data.topic} . Calling callback: ${entry.id}`, TAG);
        try {
          await entry.callback(data);
        } catch (error) {
          logger.warn(`Error on user's callback ${entry.id} topic: ${data.topic}: ${error}`, TAG);
        }
      });
    } catch (error) {
      logger.warn(`Error during handle message calling: ${error}`, TAG);
    } finally {
      // successful or not the processing has been finalized
      this.commitManager.notifyFinishedProcessing(data);
    }
  }

  /**
   * Resumes the consumer
   * @access private
   */
  resumeConsumer() {
    if (this.isPaused) {
      this.consumer.resume(this.consumer.assignments());
      this.isPaused = false;
      logger.info('Consumer resumed', TAG);
    }
  }
};
