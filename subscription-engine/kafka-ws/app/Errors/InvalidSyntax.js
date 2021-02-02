const { _WSError } = require('./WSError');
const { ErrorCodes } = require('./ErrorCodes');

/**
 * The parser has returned a syntatic error.
 */
class _InvalidSyntax extends _WSError {
  constructor() {
    super(ErrorCodes.INVALID_SYNTAX, 'invalid syntax');
  }
}

module.exports = { _InvalidSyntax };
