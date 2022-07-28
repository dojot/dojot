const crypto = require('crypto');

const PathValidatorUtil = require('../../../src/utils/path-validator-util');
const loggerMock = require('../../mocks/logger-mock');

describe('PathValidatorUtil', () => {
  it('Should pass validation', async () => {
    PathValidatorUtil.validate('/path/file', loggerMock, () => {});
    expect.assertions(0);
  });

  it('Should throw an error, when the path field is undefined', async () => {
    let error;
    try {
      await PathValidatorUtil.validate(undefined, loggerMock, () => {});
    } catch (e) {
      error = e;
    }
    expect.assertions(2);
    expect(error.responseJSON.error).toEqual('The "path" field is required.');
    expect(error.responseJSON.detail).toEqual('The "path" field is required.');
  });

  it('Should throw an error, when the length of the "path" field is less than 3 characters', async () => {
    let error;
    try {
      await PathValidatorUtil.validate('12', loggerMock, () => {});
    } catch (e) {
      error = e;
    }
    expect.assertions(2);
    expect(error.responseJSON.error).toEqual('The "path" field is invalid.');
    expect(error.responseJSON.detail).toEqual('The value in the "path" field must be between 3 and 100 characters.');
  });

  it('Should throw an error, when the length of the "path" field is greater than 100 characters', async () => {
    let error;
    try {
      await PathValidatorUtil.validate(
        crypto.randomBytes(101).toString('hex'),
        loggerMock,
        () => {},
      );
    } catch (e) {
      error = e;
    }
    expect.assertions(2);
    expect(error.responseJSON.error).toEqual('The "path" field is invalid.');
    expect(error.responseJSON.detail).toEqual('The value in the "path" field must be between 3 and 100 characters.');
  });

  it('Should throw an error, when the value of the "path" field is "/.tmp/" ', async () => {
    let error;
    try {
      await PathValidatorUtil.validate('/.tmp/', loggerMock, () => {});
    } catch (e) {
      error = e;
    }
    expect.assertions(2);
    expect(error.responseJSON.error).toEqual('The "path" field is invalid.');
    expect(error.responseJSON.detail).toEqual('The value in the "path" field is reserved.');
  });
});
