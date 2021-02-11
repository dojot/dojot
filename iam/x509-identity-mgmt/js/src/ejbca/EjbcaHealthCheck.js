const http = require('http');

async function check(url, timeout, logger) {
  return new Promise((resolve) => {
    const req = http.get(url, { timeout }, (res) => {
      let data = '';

      /* A chunk of data has been recieved. */
      res.on('data', (chunk) => {
        data += chunk;
      });

      /* The whole response has been received. Print out the result. */
      res.on('end', () => {
        if (data === 'ALLOK') {
          resolve(true);
        } else {
          resolve(false);
        }
      });
    }).on('error', (ex) => {
      logger.debug('EjbcaHealthCheck - Connection error', ex);
      resolve(false);
    }).on('timeout', () => {
      req.destroy(new Error('EjbcaHealthCheck - Connection timeout'));
      resolve(false);
    });
  });
}

class EjbcaHealthCheck {
  constructor({ url, delay, logger }) {
    Object.defineProperty(this, 'url', { value: url });
    Object.defineProperty(this, 'delay', { value: delay });
    Object.defineProperty(this, 'logger', { value: logger });
  }

  async run(signalReady, signalNotReady) {
    const result = await check(this.url, this.delay, this.logger);
    if (result) {
      signalReady();
    } else {
      signalNotReady();
    }
  }
}

module.exports = EjbcaHealthCheck;
