/**
 * Centralizes the Errors directory objects into a single one for import simplicity sake.
 */
const { _InvalidSyntax } = require('./InvalidSyntax');
const { _InvalidOperator } = require('./InvalidOperator');
const { _InvalidOperatorArity } = require('./InvalidOperatorArity');
const { _InvalidEscapeValue } = require('./InvalidEscapeValue');
const { _InvalidValue } = require('./InvalidValue');
const { _WSError } = require('./WSError');

const { ErrorCodes } = require('./ErrorCodes');

const Errors = {
  WSError: _WSError,
  InvalidEscapeValue: _InvalidEscapeValue,
  InvalidOperator: _InvalidOperator,
  InvalidOperatorArity: _InvalidOperatorArity,
  InvalidSyntax: _InvalidSyntax,
  InvalidValue: _InvalidValue,
};

module.exports = { Errors, ErrorCodes };
