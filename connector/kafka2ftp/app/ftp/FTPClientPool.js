const { logger } = require('@dojot/dojot-module-logger');
const util = require('util');
const FTPClient = require('./FTPClient.js');
const { sleep } = require('../Utils');

/**
 * A pool of FTPClient instances.
 * The pool can be used to control the number of open FTP connections,
 * reuse these connections and upload files.
 */
class FTPClientPool {
  /**
   * @param {*} configConnection {"host": "172.17.0.2",
   *            "port": 21,"secure": false,"secureOptions": null,
   *            "user": "dojot","password": "dojot","maxConcurrentConnections": 10, "remoteDir":"/"}
   * @param {*} poolId
   */
  constructor(configConnection, poolId = '') {
    const { maxConcurrentConnections, remoteDir, ...ftpConfig } = configConnection;
    this.ftpConfig = ftpConfig;
    this.maxConnec = maxConcurrentConnections;
    this.remoteDir = remoteDir;

    this.ftpConnections = [];
    this.currentIndex = -1;

    this.tagLogger = { filename: `kafka2ftp:app/FTPClientPool.${poolId}` };

    logger.info(`constructor: create ${this.maxConnec} intances of ftp`, this.tagLogger);
    logger.debug(`constructor: ftp instance configuration ${util.inspect(this.ftpConfig, { depth: null })}`, this.tagLogger);

    for (let i = 0; i < this.maxConnec; i += 1) {
      this.ftpConnections.push(new FTPClient(this.ftpConfig, `${poolId}.${i + 1}`));
    }
  }

  /**
   * Initializes ftp connection pool
   */
  async initConnections() {
    logger.info('initConnections: Initializes FTP connection pool', this.tagLogger);
    // To connect ftp clients in parallel with promises, foreach does not return promises.
    let someError = false;
    await Promise.all(this.ftpConnections.map(async (ftpConnection) => {
      await ftpConnection.connect().catch((error) => {
        someError = true;
        logger.error(`initConnections: Initializes ftp connection pool ${error.stack}`, this.tagLogger);
      });
    }));

    if (someError) {
      logger.error('initConnections: Can\'t init some connections', this.tagLogger);
      throw new Error("initConnections: Can't init connections");
    }
  }

  /**
  * Get the next available connection instance similar to a circular list.
  * @private
  */
  async nextConnection() {
    this.nextConnectionIndex();

    const ftpConnection = this.ftpConnections[this.currentIndex];

    // Checks if the ftp connection is currently being used, if so try to get another one.
    if (ftpConnection === undefined || ftpConnection.isSendingFile()) {
      logger.debug('nextConnectionIndex: Connection is currently being used,waiting 0.5 seconds to try again.', this.tagLogger);
      await sleep(500);
      return this.nextConnection();
    }

    // hold connection
    ftpConnection.setSendingFile(true);
    return ftpConnection;
  }

  /**
   * Get the next available connection id similar to a circular list.
   * @private
   */
  nextConnectionIndex() {
    if (this.currentIndex === -1) {
      this.currentIndex = +1;
    } else {
      this.currentIndex = this.currentIndex < this.ftpConnections.length - 1
        ? this.currentIndex + 1 : 0;
    }
    logger.debug(`nextConnectionIndex: The next possible connection ID is ${this.currentIndex}`, this.tagLogger);
  }

  /**
   * Destroys all FTP connections
   */
  async desployConnections() {
    logger.debug('desployConnections: Destroys all FTP connections', this.tagLogger);
    this.ftpConnections.forEach((ftpConnection) => {
      ftpConnection.disconnect();
    });
  }

  /**
   * Upload data from a readable stream to a remote file.
   * If such a file already exists it will be overwritten.
   * @param {String} filename
   * @param {ReadStream} stream
   */
  uploadFile(filename, stream) {
    logger.debug(`uploadFile: Sending ${filename}`, this.tagLogger);

    this.nextConnection().then((ftpConnectCurrent) => {
      logger.debug('uploadFile: Get an available connection', this.tagLogger);
      ftpConnectCurrent.upload(this.remoteDir + filename, stream).then(() => {
        // release connect
        ftpConnectCurrent.setSendingFile(false);
        logger.debug(`uploadFile: File uploaded ${filename} successfully`, this.tagLogger);
      }).catch((error) => {
        // release connect
        ftpConnectCurrent.setSendingFile(false);
        logger.error(`uploadFile: Error when trying to upload file${filename}: ${error.stack}`, this.tagLogger);
      });
    });
  }
}

module.exports = FTPClientPool;
