const TypeConverter = require('./TypeConverter');
const { isTypeValid } = require('../Utils');

/**
 * Returns the parsed line
 *
 * @param {string} line string in the format <param>[:<type>]=<value>
 *
 * @returns {{parameter: string, type: string, value: string} | Error}
 */
const parseLine = (line) => {
  // Splits the line in <firstPart>=<value>
  const [firstPart, value] = line.trim().split('=');
  // Checks if there is a type in the string
  if (firstPart.match(/:/g)) {
    // Separate the firstPart in <parameter>:<type>
    const [parameter, type] = firstPart.split(':');

    if (isTypeValid(type)) {
      return { parameter, type, value };
    }
    throw new Error('invalid type');
  }
  return { parameter: firstPart, type: 'string', value };
};

/**
 * Applies a type to a parsed line of configuration, applying the type from a comparation
 * configuration line if present.
 *
 * @param {{parameter: string, type: string, value: string}} line
 * @param {{parameter: string, type: string, value: string} | undefined} comparator
 *
 * @returns {{parameter: string, value: any}}
 */
const applyType = (line, comparator = undefined) => {
  const parsedLine = line;

  if (comparator) {
    parsedLine.type = comparator.type;
  }

  const convertedValue = TypeConverter[parsedLine.type](parsedLine.value);
  return { parameter: parsedLine.parameter, value: convertedValue };
};

/**
 * Merges `line` with its corresponding object in `defaultConfig`, applying the type from the
 * latter.
 *
 * @param {{parameter: string, type: string, value: string}} line
 * @param {{parameter: string, type: string, value: string}[]} defaultConfig
 *
 * @returns {{parameter: string, value: any}}
 */
const mapToTyped = (line, defaultConfig) => {
  // Finding a object with the same parameter
  const comparator = defaultConfig.find(
    (defaultLine) => defaultLine.parameter === line.parameter,
  );
  const lineWithType = applyType(line, comparator);

  return lineWithType;
};

module.exports = { applyType, mapToTyped, parseLine };
