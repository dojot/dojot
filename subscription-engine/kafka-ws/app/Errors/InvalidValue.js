const { _WSError } = require('./WSError');
const { ErrorCodes } = require('./ErrorCodes');

/**
 * An invalid escape value has been passed to the condition.
 */
class _InvalidValue extends _WSError {
  constructor(operator, value) {
    super(ErrorCodes.INVALID_VALUE, `invalid value ${value} for operator '${operator}'`);
  }
}

module.exports = { _InvalidValue };
