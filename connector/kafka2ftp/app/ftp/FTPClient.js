const { Client } = require('basic-ftp');
const util = require('util');
const { Logger } = require('@dojot/microservice-sdk');
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
    this.logger = new Logger(`kafka2ftp:app/FTPClient.${connectionName}`);

    // Use 0 to disable timeouts
    this.client = new Client(0);

    this.logger.info('Init ftp connection...');
    this.logger.debug(`constructor: ...with config: ${util.inspect(config, { depth: null })}`);

    const boundLoggerDebug = this.logger.debug.bind(this);

    this.client.ftp.verbose = boundLoggerDebug;
    this.client.ftp.log = boundLoggerDebug;
  }

  /**
   * Checks if it is in an upload process
   */
  isSendingFile() {
    this.logger.debug(`isSendingFile:  ${this.isSending ? 'yes' : 'no'}`);
    return this.isSending;
  }

  /**
   * Set if it is in an upload process
   */
  setSendingFile(isUploading) {
    this.logger.debug(`setSendingFile: old value ${this.isSending ? 'yes' : 'no'}`);
    this.isSending = isUploading;
    this.logger.debug(`setSendingFile: new value ${this.isSending ? 'yes' : 'no'}`);
  }

  /**
   * If the end (timeout) event happens, try to reconnect.
   * @private
   */
  handleEventEndOnSocketAndReconnect() {
    this.client.ftp.socket.on('end', async () => {
      this.logger.info('handleEventEndOnSocketAndReconnect: ... Connection ended');
      this.logger.debug('handleEventEndOnSocketAndReconnect: Reconnection...');
      await this.connect();
      this.logger.info('handleEventEndOnSocketAndReconnect: ...Reconnected');
    });
  }

  /**
   * Connects to the FTP Server
   *
   */
  async connect() {
    this.logger.debug('connect: Trying connect ');

    try {
      const response = await this.client.access(this.config);

      this.logger.debug(`connect: Reponse in connect:  ${util.inspect(response, { depth: null })} `);

      const { code } = response;
      if (code >= 200 && code < 300) {
        this.handleEventEndOnSocketAndReconnect();
        this.logger.info('connect: ...Connected');
      } else {
        throw new Error('connect: No connected');
      }
    } catch (error) {
      this.logger.error(`connect: Catch: ${error.stack}`);
      throw error;
    }
  }

  /**
   * Disconnects from FTP
   */
  disconnect() {
    this.logger.debug('disconnect: Disconnecting...');
    this.client.close();
    this.logger.debug('disconnect: Disconnected...');
  }

  /**
   * Checks if is connected
   */
  isConnected() {
    this.logger.debug(`isConnected: ${!this.client.closed ? 'yes' : 'no'}`);
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
        this.logger.debug('upload: Reconnect...');
        await this.connect();
        this.logger.debug('upload: Reconnected...');
      }

      this.logger.debug(`upload: Uploading... ${filename}`);
      const response = await this.client.uploadFrom(stream, filename);
      const { code } = response;
      this.logger.debug(`upload: Reponse in upload: ${util.inspect(response, { depth: null })}`);
      if (code >= 200 && code < 300) {
        this.logger.debug('upload: ...uploaded');
      } else {
        throw new Error('upload: Not uploaded');
      }
    } catch (error) {
      if (isRetry) {
        this.logger.debug('upload: Will throw a error');
        this.logger.error(`upload: Caught ${filename}: ${error.stack}`);
        throw error;
      } else {
        this.logger.warn(`upload: Caught ${filename}: ${error.stack}`);
        this.logger.debug('upload: Will retry');
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
      this.logger.debug('uploadRetry: Retrying connect.... ');
      await this.connect();
      this.logger.debug('uploadRetry: ... connected ');
      this.logger.info(`uploadRetry: Retrying upload ${filename}... `);
      await this.upload(filename, stream);
      this.logger.debug('uploadRetry: ...uploaded ');
    }, {
      retries: numberOfRetries,
    }).catch((error) => {
      this.logger.error(`uploadRetry: Caught ${filename}: ${error.stack}`);
    });
  }
}

module.exports = FTPClient;
