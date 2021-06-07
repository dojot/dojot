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
      // Emitted when the underlying socket times out from inactivity.
      // This only notifies that the socket has been idle, the request
      // must be aborted manually...

      // Deprecated since: v14.1.0, v13.14.0
      req.abort();
      // when we evolve the version of Node.js to 14.x LTS,
      // we should use .destroy() instead of .abort():
      // req.destroy(new Error('EjbcaHealthCheck - Connection timeout'));

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
