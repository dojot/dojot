const fs = require('fs');
const path = require('path');

/**
 * @overview Internal utility functions for being used in the logging module.
 */

/**
 * Parses and extracts file and line info from the call stack at the given index.
 * @param {number} index - index in the call stack.
 * @return {object} extracted file and line.
 */
function extractFileAndLineFromStack(index) {
  // get call stack, and analyze it
  // get all file, method, and line numbers
  const stackList = (new Error()).stack.split('\n').slice(3);

  // stack trace format:
  // https://v8.dev/docs/stack-trace-api
  const stackReg1 = /at\s+(.*)\s+\((.*):(\d*):(\d*)\)/gi;
  const stackReg2 = /at\s+()(.*):(\d*):(\d*)/gi;

  const s = stackList[index] || stackList[0];
  const sp = stackReg1.exec(s) || stackReg2.exec(s);
  // sp[0]: call stack line
  // sp[1]: method
  // sp[2]: filename
  // sp[3]: line
  // sp[4]: column
  if (sp && sp.length === 5) {
    return {
      file: sp[2],
      line: sp[3],
    };
  }
  return null;
}

/**
 * Adds line and number info to the given log message.
 * @param {string} message log message
 * @param {string} message log message with line and number info.
 */
function addFileAndLineToMetadata(metadata) {
  // Probably the logging call is at index 2 of the call stack
  const stackInfo = extractFileAndLineFromStack(2);
  if (stackInfo) {
    const newMetadata = metadata;
    newMetadata.file = stackInfo.file;
    newMetadata.line = stackInfo.line;
    return newMetadata;
  }
  return metadata;
}

function getRootPackageName() {
  // 1) if the application starts with 'npm start', it will get the
  // package name from the env
  let rootPackageName = process.env.npm_package_name;

  // 2) try to locate the root package.json in the directory
  // where the main application is running
  if (!rootPackageName) {
    const pjsonDir = path.dirname(require.main.filename);
    const pjsonPath = path.join(pjsonDir, 'package.json');
    try {
      const pjsonObj = JSON.parse(fs.readFileSync(pjsonPath));
      rootPackageName = pjsonObj.name;
    } catch (err) {
      rootPackageName = null;
    }
  }

  return rootPackageName;
}

module.exports = {
  addFileAndLineToMetadata, getRootPackageName,
};
