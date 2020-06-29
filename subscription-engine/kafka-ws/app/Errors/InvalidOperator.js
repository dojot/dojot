const { _WSError } = require('./WSError');
const { ErrorCodes } = require('./ErrorCodes');

/**
 * An invalid operator has been passed to the parser.
 */
class _InvalidOperator extends _WSError {
  constructor(operator) {
    super(ErrorCodes.INVALID_OPERATOR, `invalid operator '${operator}'`);
  }
}

module.exports = { _InvalidOperator };
