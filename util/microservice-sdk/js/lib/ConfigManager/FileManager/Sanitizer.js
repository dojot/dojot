/**
 * Removes commented and empty lines from the configuration files.
 *
 * @param {string} data
 *
 * @returns {string[]}
 */
const sanitize = (data) => {
  // Separating the lines in an array
  const lines = data.split('\n');
  const trimmedLines = lines.map((value) => value.trim());
  // Removing all commented and empty files
  const filteredLines = trimmedLines.filter((line) => {
    return line !== '' && line.charAt(0) !== '#';
  });
  return filteredLines;
}

module.exports = { sanitize };
