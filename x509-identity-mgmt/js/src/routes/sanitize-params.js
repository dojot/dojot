module.exports = {
  fingerprint(req, res, next, value, param) {
    let satinizedValue = value.toUpperCase();
    if (!satinizedValue.includes(':')) {
      satinizedValue = satinizedValue.match(/.{2}/g).join(':');
    }
    req.params[param] = satinizedValue;
    next();
  },
};
