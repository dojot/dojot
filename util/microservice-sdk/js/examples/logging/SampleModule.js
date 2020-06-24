const { Logger } = require('@dojot/microservice-sdk');

// Instantiate a logger for the sub-module
// The following custom name: sample-logging-module
const logger = new Logger('sample-logging-module');

// Log sample messages every five seconds
function logSampleMessages() {
  // log simple message with different logging levels
  logger.debug('message #1');
  logger.info('message #2');
  logger.warn('message #3');
  logger.error('message #4');

  // log message with additional metadata
  logger.debug('message #9', {
    rid: '7e921802-aa06-46c7-b4ba-1f6c2812d01d',
    src_ip: '192.168.127.99',
    tenant: 'admin',
  });
  logger.info('message #10', {
    rid: '7e921802-aa06-46c7-b4ba-1f6c2812d01d',
    src_ip: '192.168.127.99',
    tenant: 'admin',
  });
  logger.warn('message #11', {
    rid: '7e921802-aa06-46c7-b4ba-1f6c2812d01d',
    src_ip: '192.168.127.99',
    tenant: 'admin',
  });
  logger.error('message #12', {
    rid: '7e921802-aa06-46c7-b4ba-1f6c2812d01d',
    src_ip: '192.168.127.99',
    tenant: 'admin',
  });
}

module.exports = {
  logSampleMessages,
};
