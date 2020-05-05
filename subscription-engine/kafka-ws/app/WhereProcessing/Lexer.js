const moo = require('moo');

const { InvalidOperator } = require('../Errors').Errors;
const Utils = require('../Utils');

/**
 * Lexer to generate tokens before parsing.
 */
const Lexer = moo.compile({
  parameter: {
    match: /[a-zA-Z0-9]+[.[a-zA-Z0-9]+]*=/,
    // Removing =
    value: (matched) => matched.slice(0, -1),
  },
  operator: {
    match: /[a-z]+:/,
    // Removing :
    value: (matched) => {
      const opr = matched.slice(0, -1);
      if (Utils.operators.includes(opr)) {
        return opr;
      }
      throw new InvalidOperator(opr);
    },
  },
  values: {
    match: /.+?;/,
    // Removing ;
    value: (matched) => matched.slice(0, -1),
  },
});

module.exports = Lexer;
