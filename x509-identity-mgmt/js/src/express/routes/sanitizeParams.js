module.exports = {
  fingerprint(req, res, next, value, param) {
    let satinizedValue = value.toUpperCase();
    if (!satinizedValue.includes(':')) {
      satinizedValue = satinizedValue.match(/.{2}/g).join(':');
    }
    req.params[param] = satinizedValue;
    next();
  },
  /* Regular expression to identify certificates in a string */
  certRegExp: /-{5}BEGIN CERTIFICATE-{5}(\r\n|\r|\n)([-A-Za-z0-9+/=]{1,64}(\r\n|\r|\n))+-{5}END CERTIFICATE-{5}(\r\n|\r|\n)?/g,
};
