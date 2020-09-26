const path = require('path');
const express = require('express');
const serveIndex = require('serve-index');

/* The express.static serves the file contents
 * The serveIndex is this module serving the directory */
const mainDir = path.dirname(require.main.filename);
const schemasDir = path.join(mainDir, 'schemas');
module.exports = () => ({
  name: 'static-file-controller',
  path: '/api/v1/schemas',
  middleware: [
    express.static(schemasDir),
    serveIndex(schemasDir),
  ],
});
