const http = require('http');

async function check(url, timeout) {
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
    }).on('error', () => {
      resolve(false);
    }).on('timeout', () => {
      req.abort();
      resolve(false);
    });
  });
}

class EjbcaHealthCheck {
  constructor({ url, delay }) {
    Object.defineProperty(this, 'url', { value: url });
    Object.defineProperty(this, 'delay', { value: delay });
  }

  async run(signalReady, signalNotReady) {
    const result = await check(this.url, this.delay);
    if (result) {
      signalReady();
    } else {
      signalNotReady();
    }
  }
}

module.exports = EjbcaHealthCheck;
