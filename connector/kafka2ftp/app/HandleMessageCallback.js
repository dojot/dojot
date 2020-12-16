
const { Logger } = require('@dojot/microservice-sdk');
const util = require('util');
const ReadStream = require('./ReadStream');
const { extractPayloadFileInfo } = require('./Utils');


const logger = new Logger('kafka2ftp:app/HandleMessage');

/**
 * Creates callback to link kafka messages with
 * file uploads related to your topics on the ftp server.
 * @param {function} callbackPushFile
 */
const createCallbackToHandleMsgAndUpload = (callbackPushFile) => (data) => {
  logger.debug(`createCallbackToHandleMsgAndUpload: Message received from Kafka:  ${util.inspect(data, { depth: null })} `);

  try {
    const { filename, encoding, content } = extractPayloadFileInfo(data);

    const buffer = Buffer.from(content, encoding);
    const stream = new ReadStream(buffer);

    callbackPushFile(filename, stream);
  } catch (error) {
    logger.error(`createCallbackToHandleMsgAndUpload: Caught an error ${error.stack}`);
    throw error;
  }
};


module.exports = {
  createCallbackToHandleMsgAndUpload,
};
