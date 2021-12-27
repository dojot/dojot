const { Logger } = require('@dojot/microservice-sdk');
const util = require('util');
const FTPClient = require('./FTPClient');
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

    this.logger = new Logger(`kafka2ftp:app/FTPClientPool.${poolId}`);

    this.logger.info(`constructor: create ${this.maxConnec} instances of ftp`);
    this.logger.debug(`constructor: ftp instance configuration ${util.inspect(this.ftpConfig, { depth: null })}`);

    for (let i = 0; i < this.maxConnec; i += 1) {
      this.ftpConnections.push(new FTPClient(this.ftpConfig, `${poolId}.${i + 1}`));
    }
  }

  /**
   * Initializes ftp connection pool
   */
  async initConnections() {
    this.logger.info('initConnections: Initializes FTP connection pool');
    // To connect ftp clients in parallel with promises, foreach does not return promises.
    let someError = false;
    await Promise.all(this.ftpConnections.map(async (ftpConnection) => {
      await ftpConnection.connect().catch((error) => {
        someError = true;
        this.logger.error(`initConnections: Initializes ftp connection pool ${error.stack}`);
      });
    }));

    if (someError) {
      this.logger.error('initConnections: Can\'t init some connections');
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
      this.logger.debug('nextConnectionIndex: Connection is currently being used,waiting 0.5 seconds to try again.');
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
    this.logger.debug(`nextConnectionIndex: The next possible connection ID is ${this.currentIndex}`);
  }

  /**
   * Destroys all FTP connections
   */
  async destroyConnections() {
    this.logger.debug('destroyConnections: Destroys all FTP connections');
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
    this.logger.debug(`uploadFile: Sending ${filename}`);

    this.nextConnection().then((ftpConnectCurrent) => {
      this.logger.debug('uploadFile: Get an available connection');
      ftpConnectCurrent.upload(this.remoteDir + filename, stream).then(() => {
        // release connect
        ftpConnectCurrent.setSendingFile(false);
        this.logger.debug(`uploadFile: File uploaded ${filename} successfully`);
      }).catch((error) => {
        // release connect
        ftpConnectCurrent.setSendingFile(false);
        this.logger.error(`uploadFile: Error when trying to upload file${filename}: ${error.stack}`);
      });
    });
  }
}

module.exports = FTPClientPool;
