const express = require('express');

/* Use a middleware that parses JSON and only looks at requests
 * where the Content-Type header matches the type "application/json"
 * It also controls the maximum request body size by "limit" option,
 * the value is passed for parsing to the Bytes Library:
 * https://www.npmjs.com/package/bytes */
module.exports = ({ config }) => ({
  name: 'json-body-parsing-controller',
  middleware: express.json({ limit: config.limit }),
});
