/**
 * Base class for all custom WebSocket errors in this project.
 *
 * For a list of WebSocket error codes:
 * https://www.iana.org/assignments/websocket/websocket.xml#close-code-number
 *
 * @param {number} code must be in the interval [4000, 4999]
 * @param {string} reason
 */
class _WSError extends Error {
  constructor(code, reason) {
    super();
    if (code < 4000 || code > 4999) {
      throw new Error('invalid WebSocket custom error code');
    }
    this.ws_code = code;
    this.ws_reason = reason;
  }
}

module.exports = { _WSError };
