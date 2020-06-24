const { Logger } = require('@dojot/microservice-sdk');

const nearley = require('nearley');

const Utils = require('../Utils');
const whereParser = require('./Parser');
const {
  InvalidSyntax,
  InvalidEscapeValue,
  InvalidOperatorArity,
  InvalidValue,
} = require('../Errors').Errors;

const logger = new Logger();

/**
 * Parses the `where` query parameter.
 *
 * @param {string} where where parameter passed in the URL
 *
 * @returns {(where: string) => {parameter: string, operator: string, values: string[]}[]} Parsing
 * function that processes the where filter and returns its conditions.
 */
const WhereParser = () => {
  /**
   * Formats the values depending on the operator.
   *
   * @param {string[]} values
   * @param {string} operator
   *
   * @returns {[]} array of values in the correct format.
   */
  this.formatValues = (values, operator) => {
    let processedValues = [];

    if (Utils.numericOperators.includes(operator)) {
      // No more than one value is permitted for a numeric operator
      if (values.length > 1) {
        throw new InvalidOperatorArity(operator, values.length);
      }
      if (!Utils.isNumber(values[0])) {
        throw new InvalidValue(operator, values[0]);
      }
      processedValues.push(Number(values[0]));
    } else if (Utils.setOperators.includes(operator)) {
      // Multiple values are permitted for set operators
      processedValues = values.map((value) => value.toString());
    }

    return processedValues;
  };

  /**
   * Escapes all `\` in the value.
   *
   * @param {string} value string to be escaped.
   *
   * @returns {string} escaped string.
   */
  this.escapeString = (value) => {
    let escapedValue = '';

    for (let i = 0; i < value.length; i += 1) {
      // If the value is '\', we are treating a escaped value
      if (value[i] === '\\') {
        // Treating escaped values that are letters
        if (value[i + 1].match(/[a-zA-Z]/)) {
          /*
           * If we only copy the escaped char (like \n), it won't be treated as a escaped one,
           * so we need to store the escaped value by itself and then add to our string
          */
          const escapeChar = Utils.escapeChars[value[i + 1]];
          if (escapeChar) {
            escapedValue += escapeChar;
          } else {
            throw new InvalidEscapeValue(`\\${value[i + 1]}`);
          }
        // If it is not a letter, the treatment is simple
        } else {
          escapedValue += value[i + 1];
        }

        // Skips to the escaped value, so the loop increment can skip it too
        i += 1;
      // Copy any value other than '\'
      } else {
        escapedValue += value[i];
      }
    }

    return escapedValue;
  };

  /**
   * Extracts the unquoted values.
   *
   * @param {string} values all values passed to the parser.
   * @param {string[]} quotedValues array of the quoted values without quotes.
   *
   * @returns {string[]} an array of strings with the unquoted values.
   */
  this.extractUnquotedValues = (values, quotedValues) => {
    let withoutQuotes = values;

    // Removing the quoted values from the original string to extract the other values
    for (let i = 0; i < quotedValues.length; i += 1) {
      withoutQuotes = withoutQuotes.replace(`"${quotedValues[i]}"`, '');
    }

    withoutQuotes = withoutQuotes
      .split(',') // separating into individual attributes
      .filter((val) => val.length > 0); // removing eventual empty strings

    return withoutQuotes;
  };

  /**
   * Extracts the quoted values.
   *
   * @param {string} values all values passed to the parser.
   *
   * @returns {string[]} an array of strings with quoted values.
   */
  this.extractQuotedValues = (values) => {
    let value = ''; // the value that is inside quotes
    const processedValues = [];

    for (let i = 0; i < values.length; i += 1) {
      // If there is something in value, it means that this value is inside quotes
      if (value.length > 0) {
        // If the char is an unescaped quote, then this is the end of the value
        if (values[i].match(/"/)
            && i - 1 > 0
            && !values[i - 1].match(/\\/)) {
          processedValues.push(value.slice(1));
          value = '';
        // Append the char otherwise
        } else {
          value += values[i];
        }
      // If value is empty and the character is ", this is the beginning of a value inside ""
      } else if (values[i].match(/"/)) {
        value = values[i];
      }
    }

    return processedValues;
  };

  /**
   * Extracts the quoted and unquoted values.
   *
   * @param {string} values
   *
   * @returns {string[]} processed values in an array
   */
  this.separateValues = (values) => {
    let processedValues = [];

    if (!values.match(/.*?".*?/)) {
      processedValues = values.split(',');
    } else {
      // Separating attributes with quotes
      const quotedValues = this.extractQuotedValues(values);
      // Separating attributes without quotes
      const withoutQuotes = this.extractUnquotedValues(values, quotedValues);

      // The order of filtering here is important: we only escape values that are inside quotes
      processedValues = quotedValues
        .map((val) => this.escapeString(val))
        .concat(withoutQuotes); // appending the unquoted values
    }

    return processedValues;
  };

  /**
   * Process the condition, separating its values in an array with each individual value.
   *
   * @param {{parameter: string, operator: string, values: string}} condition
   *
   * @returns {{parameter: string, operator: string, values: []}} a Condition object with the
   * processed condition.
   */
  this.processCondition = (condition) => {
    const { parameter, operator, values } = condition;

    let processedValues = this.separateValues(values);
    processedValues = this.formatValues(processedValues, operator);

    return { parameter, operator, values: processedValues };
  };

  return (where) => {
    // We need to make a new parser for every parsing, but it is very cheap
    const parser = new nearley.Parser(nearley.Grammar.fromCompiled(whereParser));

    try {
      parser.feed(where);
    } catch (error) {
      if (String(error.message).includes('invalid syntax')) {
        logger.error(error.message);
        throw new InvalidSyntax();
      } else {
        throw error;
      }
    }

    const conditions = parser.results[0].map((condition) => this.processCondition(condition));

    return conditions;
  };
};

module.exports = { WhereParser };
