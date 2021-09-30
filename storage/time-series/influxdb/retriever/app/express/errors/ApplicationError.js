/**
 * Custom error for aplication
 */
class ApplicationError extends Error {
  constructor(message, code = 500) {
    super(message);
    this.code = code;
  }

  /*
  * Returns error code. If the code is undefined, it will return code 500
  * This method is necessary to handle the original js error,
  * because the original error does not have the code property.
  */
  static handleCode(code) {
    return code || 500;
  }
}

module.exports = ApplicationError;
