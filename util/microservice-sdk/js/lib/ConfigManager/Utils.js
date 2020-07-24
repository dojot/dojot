const Types = ['boolean', 'float', 'integer', 'string', 'string[]'];

/**
 * Transforms the value and key into the canonical form for file storage.
 *
 * @param {string} value
 * @param {string} key
 */
const toCanonicalFileFormat = (value, key) => `${key}=${value}`;

/**
 * Creates the configuration filename in the format `<path>/<filename>`.
 *
 * @param {string} filename
 * @param {string} path
 *
 * @returns {string}
 */
const createFilename = (filename, path) => `${path}/${filename.toLowerCase()}`;

/**
 * Checks if the passed type is a valid one (case insensitive).
 *
 * @param {string} type
 */
const isTypeValid = (type) => Types.some((acceptedType) => acceptedType === type.toLowerCase());

module.exports = {
  createFilename,
  toCanonicalFileFormat,
  isTypeValid,
};
