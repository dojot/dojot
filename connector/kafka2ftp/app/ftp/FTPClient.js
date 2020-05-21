const { Client } = require('basic-ftp');
const util = require('util');
const { logger } = require('@dojot/dojot-module-logger');
const retry = require('async-retry');
const { retryOptions: { retries: numberOfRetries } } = require('../Config');

/**
 * Create a single FTP connection and send a file over this connection.
 * If the connection is not reached or the sending fails,
 * try to reconnect/resend in an "exponential backoff" strategy.
 */
class FTPClient {
  /**
   *
   * @param {*} config Connect Options Ex.:
   *            {host: "myftpserver.com", user: "very",
   *            password: "password", secure: true,
   *            port:21, secureOptions: null}
   * @param {*} connectionName A name to identify the connection in the log
   */
  constructor(config, connectionName = '') {
    this.config = config;
    this.connectionName = connectionName;
    this.isSending = false;
    this.tagLogger = { filename: `kafka2ftp:app/FTPClient.${connectionName}` };

    // Use 0 to disable timeouts
    this.client = new Client(0);

    logger.info('Init ftp connection...', this.tagLogger);
    logger.debug(`constructor: ...with config: ${util.inspect(config, { depth: null })}`, this.tagLogger);

    this.client.ftp.verbose = logger.debug;
    this.client.ftp.log = logger.debug;
  }

  /**
   * Checks if it is in an upload process
   */
  isSendingFile() {
    logger.debug(`isSendingFile:  ${this.isSending ? 'yes' : 'no'}`, this.tagLogger);
    return this.isSending;
  }

  /**
   * Set if it is in an upload process
   */
  setSendingFile(isUploading) {
    logger.debug(`setSendingFile: old value ${this.isSending ? 'yes' : 'no'}`, this.tagLogger);
    this.isSending = isUploading;
    logger.debug(`setSendingFile: new value ${this.isSending ? 'yes' : 'no'}`, this.tagLogger);
  }

  /**
   * If the end (timeout) event happens, try to reconnect.
   * @private
   */
  handleEventEndOnSocketAndReconnect() {
    this.client.ftp.socket.on('end', async () => {
      logger.info('handleEventEndOnSocketAndReconnect: ... Connection ended', this.tagLogger);
      logger.debug('handleEventEndOnSocketAndReconnect: Reconnection...', this.tagLogger);
      await this.connect();
      logger.info('handleEventEndOnSocketAndReconnect: ...Reconnected', this.tagLogger);
    });
  }

  /**
   * Connects to the FTP Server
   *
   */
  async connect() {
    logger.debug('connect: Trying connect ', this.tagLogger);

    try {
      const response = await this.client.access(this.config);

      logger.debug(`connect: Reponse in connect:  ${util.inspect(response, { depth: null })} `, this.tagLogger);

      const { code } = response;
      if (code >= 200 && code < 300) {
        this.handleEventEndOnSocketAndReconnect();
        logger.info('connect: ...Connected', this.tagLogger);
      } else {
        throw new Error('connect: No connected');
      }
    } catch (error) {
      logger.error(`connect: Catch: ${error.stack}`, this.tagLogger);
      throw error;
    }
  }

  /**
   * Disconnects from FTP
   */
  disconnect() {
    logger.debug('disconnect: Disconnecting...', this.tagLogger);
    this.client.close();
    logger.debug('disconnect: Disconnected...', this.tagLogger);
  }

  /**
   * Checks if is connected
   */
  isConnected() {
    logger.debug(`isConnected: ${!this.client.closed ? 'yes' : 'no'}`, this.tagLogger);
    return !this.client.closed;
  }

  /**
 *
 * @param {string} filename remotePath
 * @param {ReadStream} stream readableStream
 */
  async upload(filename, stream, isRetry = false) {
    try {
      if (!this.isConnected()) {
        logger.debug('upload: Reconnect...', this.tagLogger);
        await this.connect();
        logger.debug('upload: Reconnected...', this.tagLogger);
      }

      logger.debug(`upload: Uploading... ${filename}`, this.tagLogger);
      const response = await this.client.uploadFrom(stream, filename);
      const { code } = response;
      logger.debug(`upload: Reponse in upload: ${util.inspect(response, { depth: null })}`, this.tagLogger);
      if (code >= 200 && code < 300) {
        logger.debug('upload: ...uploaded', this.tagLogger);
      } else {
        throw new Error('upload: Not uploaded');
      }
    } catch (error) {
      if (isRetry) {
        logger.debug('upload: Will throw a error', this.tagLogger);
        logger.error(`upload: Caught ${filename}: ${error.stack}`, this.tagLogger);
        throw error;
      } else {
        logger.debug('upload: Will retry', this.tagLogger);
        await this.uploadRetry(filename, stream);
      }
    }
  }


  /**
   * Try to reconnect and upload in an "exponential backoff" strategy
   * @private
   * @param {string} filename
   * @param {ReadStream} stream
   */
  async uploadRetry(filename, stream) {
    await retry(async () => {
      logger.debug('uploadRetry: Retrying connect.... ', this.tagLogger);
      await this.connect();
      logger.debug('uploadRetry: ... connected ', this.tagLogger);
      logger.info(`uploadRetry: Retrying upload ${filename}... `, this.tagLogger);
      await this.upload(filename, stream);
      logger.debug('uploadRetry: ...uploaded ', this.tagLogger);
    }, {
      retries: numberOfRetries,
    }).catch((error) => {
      logger.error(`uploadRetry: Caught ${filename}: ${error.stack}`, this.tagLogger);
    });
  }
}

module.exports = FTPClient;
