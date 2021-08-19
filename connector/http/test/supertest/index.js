/**
 * Module dependencies.
 */
const methods = require('methods');
const fs = require('fs');
const https = require('https');
const Test = require('../../node_modules/supertest/lib/test');

/**
 * Test against the given `app`,
 * returning a new `Test`.
 *
 * @param {Function|Server} app
 * @return {Test}
 * @api public
 */
function requestTest(app) {
  const obj = {};
  let newApp;

  if (typeof app === 'function') {
    newApp = https.createServer(
      {
        cert: fs.readFileSync('test/certs/server/server_cert.pem'),
        key: fs.readFileSync('test/certs/server/server_key.pem'),
        ca: fs.readFileSync('test/certs/ca/ca_cert.pem'),
        rejectUnauthorized: false,
        requestCert: true,
      },
      app,
    ); // eslint-disable-line no-param-reassign
  }

  methods.forEach((method) => {
    // eslint-disable-next-line security/detect-object-injection
    obj[method] = (url) => new Test(newApp, method, url);
  });

  // Support previous use of del
  obj.del = obj.delete;

  return obj;
}

module.exports = requestTest;

/**
 * Expose `Test`
 */
module.exports.Test = Test;

/**
 * Expose the agent function
 */
module.exports.agent = require('../../node_modules/supertest/lib/agent');
