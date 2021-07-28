/**
 *
 * @param {string} accept the accept entered in the request
 * @returns {"json"|"csv"}  the format the data will be returned
 */

const getExpectedResponseFormat = (accept) => {
  try {
    // ignored because: https://eslint.org/docs/rules/prefer-named-capture-group
    //    https://github.com/eslint/eslint/issues/11381
    // eslint-disable-next-line security/detect-unsafe-regex
    const regex = new RegExp('^(aplication|text)/(?<ext>json|csv)$', 'igm');
    const matches = regex.exec(accept);

    return matches ? matches.groups.ext : 'json';
  } catch (e) {
    throw new Error('The HTTP Accept header is invalid');
  }
};

module.exports = { getExpectedResponseFormat };
