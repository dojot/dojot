
const { PreciseDate } = require('@google-cloud/precise-date');

/**
 * Check if match with RFC3339
 *
 * @param {String} dateTime
 *
 * @returns if match
 */
const checkMatchRFC3339 = (dateTime) => {
  // TODO: better understand why this regex is unsafe and change it
  // https://www.regular-expressions.info/redos.html
  // eslint-disable-next-line security/detect-unsafe-regex
  if (dateTime.match(/^(?<fullyear>\d{4})-(?<month>0[1-9]|1[0-2])-(?<mday>0[1-9]|[12][0-9]|3[01])T(?<hour>[01][0-9]|2[0-3]):(?<minute>[0-5][0-9]):(?<second>[0-5][0-9]|60)(?<secfrac>\.[0-9]+)?(Z|(\+|-)(?<offset_hour>[01][0-9]|2[0-3]):(?<offset_minute>[0-5][0-9]))$/)) {
    return true;
  }

  return false;
};

/**
 * Returns a string representing the nanoseconds in the specified date according to universal time.
 *
 * @param {String} dateTime RFC3339 string
 *
 * @returns {string}  a string representing the nanoseconds
 *
 * @throws If date is invalid or date out of range (less 1970) or doest match with RFC3339
 */
const parseDateTimeToUnixNs = (dateTime) => {
  if (!checkMatchRFC3339(dateTime)) {
    throw new Error('Date doest match with RFC3339');
  }

  const timestampNano = PreciseDate.parseFull(dateTime);

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

module.exports = { checkMatchRFC3339, parseDateTimeToUnixNs };
