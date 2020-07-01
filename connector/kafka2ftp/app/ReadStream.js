/* eslint-disable no-underscore-dangle */
const stream = require('stream');

class ReadStream extends stream.Readable {
  constructor(object) {
    super();
    stream.Readable.call(this, {});
    this._object = object;
  }

  _read() {
    this.push(this._object);
    this._object = null;
  }
}

module.exports = ReadStream;
