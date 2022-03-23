const { Writable } = require('stream');

module.exports = class ResponseMock extends Writable {
  constructor() {
    super();
    this.headers = [];
    this.body = '';
  }

  status(code) {
    this.code = code;
    return this;
  }

  json(body) {
    this.body = body;
    return this;
  }
};
