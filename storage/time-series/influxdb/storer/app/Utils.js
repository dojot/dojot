
const { PreciseDate } = require('@google-cloud/precise-date');

/**
 * Check if match with restricted
 * string described in ISO8601 with
 * formatting: YYYY-MM-DDThh:mm:ss.fffffffffZ, the fractional part (.fffffffff) being optional.
 *
 * @param {String} iso
 *
 * @returns if match
 */
const checkMatchRestrictedISO = (iso) => {
  if (iso.match(/(\d{4}-(0[1-9]|1[0-2])-(0[1-9]|1[0-9]|2[0-9]|3[0-1])T(2[0-3]|[01][0-9]):([0-5][0-9]):([0-5][0-9]))((\.\d{1,3})(\d{0,6}))?Z/)) {
    return true;
  }

  return false;
};

/**
 * Returns a string representing the nanoseconds in the specified date according to universal time.
 *
 * @param {String} fullISO full ISO string
 *
 * @returns {string}  a string representing the nanoseconds
 *
 * @throws If date is invalid or date out of range or doest match with restricted ISO8601
 */
const parseISODateToUnixNs = (fullISO) => {
  if (!checkMatchRestrictedISO(fullISO)) {
    throw new Error('Date doest match with restricted ISO8601');
  }

  const timestampNano = PreciseDate.parseFull(fullISO);

  // when try convert a invalid date,
  // the preciseDate returns a string like NaN000000NaN
  if (!timestampNano || timestampNano.includes('NaN')) {
    throw new Error('Date is invalid');
  }

  // date before 1970
  if (timestampNano.charAt(0) === '-') {
    throw new Error('Date out of range');
  }

  return timestampNano;
};

module.exports = { checkMatchRestrictedISO, parseISODateToUnixNs };
