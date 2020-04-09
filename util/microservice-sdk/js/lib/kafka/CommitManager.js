const { logger } = require('@dojot/dojot-module-logger');

const TAG = { filename: 'commit-mngr' };

/**
 * Default time interval for committing the processed messages.
 */
const DEFAULT_COMMIT_TIME_INTERVAL = 5000;

/**
 * A simple class for tracking the processing of the Kafka messages and managing the commits.
 * It calls periodically a method to consolidate the processed messages into the
 * message brokers (Kafka instances).
 */
module.exports = class CommitManager {
  /**
   * Creates a new instance of the CommitManager.
   * @param {*} commitCb the function to be called to commit the messages into Kafka.
   * @param {*} commitInterval the interval in ms that the consolidator should be called.
   */
  constructor(commitCb, commitInterval) {
    this.commitCb = commitCb;
    this.commitInterval = commitInterval || DEFAULT_COMMIT_TIME_INTERVAL;
    this.topics = {};
    this.caller = null;
  }

  /**
   * Initializes the manager. It schedules to run the consolidation of the
   * commits every 'commitInterval' ms.
   */
  init() {
    if (this.caller) {
      clearInterval(this.caller);
    }
    this.caller = setInterval(this.commitProcessedOffsets.bind(this),
      this.commitInterval);
  }

  /**
   * Notifies the manager that a new message is started to be processed.
   * ATTENTION: it should be called in order, before any async processing.
   * @param {*} data the kafka message that is starting to be processed. It is
   * an object with at least the following attributes:
   * {
   *   topic: 'librdtesting-01', // topic the message comes from
   *   partition: 1, // partition the message was on
   *   offset: 1337, // offset the message was read from
   * }.
   */
  notifyStartProcessing(data) {
    const { partition, offset, topic } = data;
    this.topics[topic] = this.topics[topic] || {};
    this.topics[topic][partition] = this.topics[topic][partition] || [];
    this.topics[topic][partition].push({
      offset,
      done: false,
    });
  }

  /**
   * Notifies the manager that a previous notified message
   * (on `notifyStartProcessing`) has been finished to be processed.
   * @param {*} data the kafka message that is finished to be processed. It is
   * an object with at least the following attributes:
   * {
   *   topic: 'librdtesting-01', // topic the message comes from
   *   partition: 1, // partition the message was on
   *   offset: 1337, // offset the message was read from
   * }.
   */
  notifyFinishedProcessing(data) {
    const { partition, offset, topic } = data;
    const topicPartitions = this.topics[topic];
    if (!topicPartitions) {
      return;
    }

    const partitionOffsets = topicPartitions[partition];
    if (!partitionOffsets) {
      return;
    }

    const record = partitionOffsets.filter(
      (currRecord) => currRecord.offset === offset,
    )[0];
    if (record) {
      record.done = true;
    }
  }

  /**
   * This method should be invoked during the Kafka's rebalance procedure.
   * It clears the work tracking.
   *
   * Note: Once a rebalance occurs, the uncommited messages will be
   * redelivered to the new assignee that is part of the consumer group.
   */
  onRebalance() {
    this.topics = {};
  }

  /**
   * Consolidates the processed messages into Kafka.
   *
   * The consolidation consists in computing the largest offset
   * which all previous messages have been processed, and commits
   * this offset. This is more efficient than committing
   * every message once kafka considers all previous messages commited
   * when an specific offset is commited.
   *
   * @access private
   */
  async commitProcessedOffsets() {
    try {
      const offsetsToCommit = [];
      const topicsToBeDeleted = [];

      Object.keys(this.topics).forEach((topic) => {
        const partitions = this.topics[topic];
        const partitionsToBeDeleted = [];
        Object.keys(partitions).forEach((partition) => {
          const partitionOffsets = partitions[partition];

          const firstProcessedMsgIndex = partitionOffsets.findIndex((record) => record.done);
          const firstNonProcessedMsgIndex = partitionOffsets.findIndex((record) => !record.done);

          if (((firstProcessedMsgIndex > firstNonProcessedMsgIndex)
              && (firstNonProcessedMsgIndex !== -1))
            || (firstProcessedMsgIndex === -1)) {
            // there is some pending task before the first completed task
            // or there is not completed tasks
            // in these cases we do not commit
            return;
          }

          let lastProcessedRecordIndex = firstNonProcessedMsgIndex - 1;
          // all tasks are finished
          if (firstNonProcessedMsgIndex === -1) {
            lastProcessedRecordIndex = partitionOffsets.length - 1;
          }
          const lastProcessedRecord = partitionOffsets[lastProcessedRecordIndex];
          offsetsToCommit.push({
            partition: Number(partition),
            offset: lastProcessedRecord.offset + 1,
            topic,
          });
          // remove committed records from array
          partitionOffsets.splice(0, lastProcessedRecordIndex + 1);
          if (partitionOffsets.length === 0) {
            partitionsToBeDeleted.push(partition);
          }
        });

        partitionsToBeDeleted.forEach((partition) => {
          delete this.topics[topic][partition];
        });
        if (Object.values(this.topics[topic]).length === 0) {
          topicsToBeDeleted.push(topic);
        }
      });

      topicsToBeDeleted.forEach((topic) => {
        delete this.topics[topic];
      });

      if (offsetsToCommit.length > 0) {
        logger.debug(`Submitting a commit to Kafka: ${JSON.stringify(offsetsToCommit)}`, TAG);
        this.commitCb(offsetsToCommit);
      }

      return Promise.resolve();
    } catch (error) {
      logger.warn(`Failed on committing offsets. ${error}`, TAG);
      return Promise.reject(error);
    }
  }
};
