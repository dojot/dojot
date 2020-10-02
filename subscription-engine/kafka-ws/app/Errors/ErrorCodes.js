/**
 * Important: every error code must have its own WSError inherited class!
 */
// TODO: enhance the errors messages
const ErrorCodes = {
  INVALID_SYNTAX: 4000,
  INVALID_OPERATOR: 4001,
  INVALID_ESCAPE_VALUE: 4002,
  INVALID_OPERATOR_ARITY: 4003,
  INVALID_VALUE: 4004,
  /**
   * TODO: this error does not seem to be used; if it is, see the next TODO
   * TODO: create a WSError inherited class for this error
   */
  INVALID_PATHNAME: 4400,
  /**
   * TODO: this error does not seem to be used; if it is, see the next TODO
   * TODO: create a WSError inherited class for this error
   */
  INVALID_TOKEN_JWT: 4401,
  FORBIDDEN_TOPIC: 4403, // TODO: create a WSError inherited class for this error
  EXPIRED_CONNECTION: 4408, // TODO: create a WSError inherited class for this error
  INTERNAL: 4999, // TODO: create a WSError inherited class for this error
  /**
   * TODO: My idea with the WSError inherited classes was to pass their codes and messages to the
   * ws.close function (and to the logger.error too), this way we can centralize these messages and
   * codes in one place, thus avoiding different messages for the same error in more than one place.
   */
};

module.exports = { ErrorCodes };
