const http = require('http');

async function check(url, timeout) {
  const allOk = await new Promise((resolve) => {
    const req = http.get(url, { timeout }, (res) => {
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
    }).on('timeout', () => {
      req.abort();
      resolve(false);
    });
  });
  return allOk;
}

class EjbcaHealthCheck {
  constructor({ healthCheck, url, delay }) {
    Object.defineProperty(this, 'healthCheck', { value: healthCheck });
    Object.defineProperty(this, 'url', { value: url });
    Object.defineProperty(this, 'delay', { value: delay });
    Object.defineProperty(this, 'interval', { value: null, writable: true });
    Object.defineProperty(this, 'busy', { value: false, writable: true });
  }

  start() {
    const fn = async () => {
      // If the EJBCA health check response takes longer than the check interval,
      // we must prevent new checks from being stacked in the Event Loop.
      if (!this.busy) {
        this.busy = true;
        const result = await check(this.url, this.delay);
        this.busy = false;
        if (result) {
          this.healthCheck.ready();
        } else {
          this.healthCheck.notReady();
        }
      }
    };

    // performs the first health check immediately
    fn();

    // The next health checks will be performed at intervals
    this.interval = setInterval(fn, this.delay);

    // https://nodejs.org/api/timers.html#timers_timeout_unref
    this.interval.unref();
  }

  stop() {
    clearInterval(this.interval);
  }
}

module.exports = EjbcaHealthCheck;
