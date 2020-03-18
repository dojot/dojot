
const { logger } = require('@dojot/dojot-module-logger');
const KafkaFTPConsumers = require('./KafkaFTPConsumers');
const { createCallbackToHandleMsgAndUpload } = require('./HandleMessageCallback');
const FTPClientPool = require('./ftp/FTPClientPool');

const TAG = { filename: 'kafka2ftp:app/Service' };

/**
 *  Initializes, disconnects and associates kafka with ftp
 */
class Service {
  /**
    * @param {*} endpoints ["tenant":"admin", "ftp": {"host": "172.17.0.2",
    *            "port": 21,"secure": false,"secureOptions": null,
    *            "user": "dojot","password": "dojot","maxConcurrentConnections": 10,
    *            "remoteDir":"/"}]
    */
  constructor(endpoints) {
    this.endpoints = endpoints;

    this.kafkaFTPConsumers = new KafkaFTPConsumers();
    this.ftpConnections = {};
  }

  /**
    * Initializes kafka and the ftp connections pool
    */
  async initService() {
    logger.info('initService: Starting service...', TAG);

    await this.kafkaFTPConsumers.init();

    let someError = false;
    await Promise.all(this.endpoints.map(async (endpoint) => {
      const { tenant, ftp } = endpoint;
      this.ftpConnections[tenant] = new FTPClientPool(ftp, tenant);

      this.ftpConnections[tenant].initConnections().catch((error) => {
        logger.error(`initService: Caught an error ${error.stack}`, TAG);
        someError = true;
      });
      const boundUpload = this.ftpConnections[tenant].uploadFile.bind(this.ftpConnections[tenant]);
      this.kafkaFTPConsumers.registerCallback(tenant,
        createCallbackToHandleMsgAndUpload(boundUpload));
    }));
    if (someError) {
      logger.error('initService: Can\'t init connections', this.tagLogger);
      throw new Error("initService: Can't init connections");
    }
  }

  /**
     * Destroy kafka callbacks,  connections with ftp services
     */
  async stopService() {
    logger.info('stopService: Stopping service... ', TAG);

    await this.kafkaFTPConsumers.unregisterCallbacks();

    await Promise.all(Object.keys(this.ftpConnections).map(async (tenant) => {
      await this.ftpConnections[tenant].desployConnections();
    }));

    // stops the process immediately
    process.abort();
  }
}


module.exports = Service;
