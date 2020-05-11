const { _WSError } = require('./WSError');
const { ErrorCodes } = require('./ErrorCodes');

/**
 * The passed operator does not accept this number of values.
 *
 * @param {string} operator
 * @param {number} n number of values
 */
class _InvalidOperatorArity extends _WSError {
  constructor(operator, n) {
    super(ErrorCodes.INVALID_OPERATOR_ARITY, `invalid arity ${n} for '${operator}'`);
  }
}

module.exports = { _InvalidOperatorArity };
