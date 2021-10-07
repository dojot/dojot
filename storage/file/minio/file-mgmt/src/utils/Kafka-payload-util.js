function getValue(payload) {
  const { value } = payload;
  const payloadObject = JSON.parse(value.toString());
  return payloadObject;
}

module.exports = {
  getValue,
};
