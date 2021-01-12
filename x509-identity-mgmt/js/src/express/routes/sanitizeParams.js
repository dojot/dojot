
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

function fingerprintHandler(req, res, next, value, param) {
  Reflect.set(req.params, param, sanitizeFingerprint(value));
  next();
}

module.exports = {
  certRegExp,
  sanitizeFingerprint,
  fingerprintHandler,
};
