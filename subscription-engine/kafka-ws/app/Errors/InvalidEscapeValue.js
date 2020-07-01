const { _WSError } = require('./WSError');
const { ErrorCodes } = require('./ErrorCodes');

/**
 * An invalid escape value has been passed to the condition.
 */
class _InvalidEscapeValue extends _WSError {
  constructor(value) {
    super(ErrorCodes.INVALID_ESCAPE_VALUE, `invalid escape value ${value}`);
  }
}

module.exports = { _InvalidEscapeValue };
