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
  // Probably the logging call is at index 1 of the call stack
  const stackInfo = extractFileAndLineFromStack(1);
  if (stackInfo) {
    const newMetadata = metadata;
    newMetadata.file = stackInfo.file;
    newMetadata.line = stackInfo.line;
    return newMetadata;
  }
  return metadata;
}

module.exports = {
  addFileAndLineToMetadata,
};
