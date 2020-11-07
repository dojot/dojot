const pathModule = require('path');
const express = require('express');
const serveIndex = require('serve-index');

/**
 * Interceptor to serving static files in Express:
 * https://expressjs.com/en/starter/static-files.html
 *
 * @param {string} baseDirectory Base directory that the process has access to (application root)
 * @param {string} staticFilePath Relative path to the directory where the static files are
 * @param {string} path URL where the interceptor will be fired
 */
function createInterceptor(baseDirectory, staticFilePath, path = '/') {
  const baseDir = pathModule.dirname(baseDirectory);
  const staticFileDir = pathModule.join(baseDir, staticFilePath);
  return {
    path,
    name: 'static-file-interceptor',
    middleware: [
      express.static(staticFileDir),
      serveIndex(staticFileDir),
    ],
  };
}

module.exports = ({
  baseDirectory, staticFilePath, path,
}) => createInterceptor(baseDirectory, staticFilePath, path);
