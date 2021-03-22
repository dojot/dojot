
/* Regular expression to identify certificates in a string */
// eslint-disable-next-line security/detect-unsafe-regex
const certRegExp = /-{5}BEGIN CERTIFICATE-{5}(\r\n|\r|\n)([-A-Za-z0-9+/=]{1,64}(\r\n|\r|\n))+-{5}END CERTIFICATE-{5}(\r\n|\r|\n)?/g;

function sanitizeFingerprint(value) {
  let satinizedValue = value.toUpperCase();
  if (satinizedValue && !satinizedValue.includes(':')) {
    satinizedValue = satinizedValue.match(/.{2}/g).join(':');
  }
  return satinizedValue;
}

/**
 * Converts from Windows-style to UNIX-style line endings.
 *
 * Microsoft Windows represent line endings as carriage return (CR) followed by line feed (LF)
 * UNIX-like operating systems represent line endings only as line feed (LF).
 *
 * @param {string} pem Certificate represented in PEM format
 * @returns the PEM with UNIX-style line endings.
 */
function sanitizeLineBreaks(pem) {
  // replaces DOS line breaks (\r\n) with UNIX line breaks (\n)
  // Then remove all remaining (the last) line breaks...
  return pem.replace(/\r\n|\r/g, '\n').replace(/\n+$/, '');
}

function fingerprintHandler(req, res, next, value, param) {
  Reflect.set(req.params, param, sanitizeFingerprint(value));
  next();
}

module.exports = {
  certRegExp,
  sanitizeFingerprint,
  fingerprintHandler,
  sanitizeLineBreaks,
};
