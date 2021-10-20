module.exports = class ResponseMock {
  status(code) {
    this.code = code;
    return this;
  }

  json(body) {
    this.body = body;
    return this;
  }
};
