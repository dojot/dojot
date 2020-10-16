const http = require('http');

async function check(url) {
  const allOk = await new Promise((resolve) => {
    http.get(url, (res) => {
      let data = '';
      /* A chunk of data has been recieved. */
      res.on('data', (chunk) => { data += chunk; });
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
    });
  });
  return allOk;
}

class EjbcaHealthCheck {
  constructor({ healthCheck, url, interval }) {
    Object.defineProperty(this, 'healthCheck', { value: healthCheck });
    Object.defineProperty(this, 'url', { value: url });
    Object.defineProperty(this, 'interval', { value: interval });
    Object.defineProperty(this, 'ref', { value: null, writable: true });
  }

  start() {
    this.ref = setInterval(async () => {
      const result = await check(this.url);
      if (result) {
        this.healthCheck.ready();
      } else {
        this.healthCheck.notReady();
      }
    }, this.interval);
  }

  stop() {
    clearInterval(this.ref);
  }
}

module.exports = EjbcaHealthCheck;
