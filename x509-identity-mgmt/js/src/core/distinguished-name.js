const { certificate: certCfg } = require('../config');

const { BadRequest } = require('./errors');

/* List of allowed attributes on SubjectDN */
const allowed = [...certCfg.subject.allowedAttrs];

/* constraints on the values of the allowed attributes on SubjectDN */
const allowedRegex = certCfg.subject.allowedAttrsConstraints;

/* List of mandatory attributes on SubjectDN */
const mandatory = [...certCfg.subject.mandatoryAttrs];

/* list of attributes to be overwritten in SubjectDN */
const { constantAttrs } = certCfg.subject;

/* A distinguished name for an X.509 certificate consists
 * of a sequence of relative distinguished names (RDN).
 * For details on these SubjectDN attributes,
 * simply search the OID Repository:
 * http://oid-info.com/get/<OID-dot-notation>
 *
 * These are the Subject RDNs supported by the EJBCA.
 * For custom RDNs, you must also configure them in the EJBCA. */
const RDNs = [
  {
    OID: '2.5.4.3',
    name: 'commonName',
    shortName: 'CN',
  }, {
    OID: '2.5.4.4',
    name: 'surname',
    shortName: 'SN',
  }, {
    OID: '2.5.4.5',
    name: 'serialNumber',
  }, {
    OID: '2.5.4.6',
    name: 'countryName',
    shortName: 'C',
  }, {
    OID: '2.5.4.7',
    name: 'localityName',
    shortName: 'L',
  }, {
    OID: '2.5.4.8',
    name: 'stateOrProvinceName',
    shortName: 'ST',
  }, {
    OID: '2.5.4.9',
    name: 'streetAddress',
  }, {
    OID: '2.5.4.10',
    name: 'organizationName',
    shortName: 'O',
  }, {
    OID: '2.5.4.11',
    name: 'organizationalUnit',
    shortName: 'OU',
  }, {
    OID: '2.5.4.12',
    name: 'title',
  }, {
    OID: '2.5.4.13',
    name: 'description',
  }, {
    OID: '2.5.4.15',
    name: 'businessCategory',
  }, {
    OID: '2.5.4.16',
    name: 'postalAddress',
  }, {
    OID: '2.5.4.17',
    name: 'postalCode',
  }, {
    OID: '2.5.4.20',
    name: 'telephoneNumber',
  }, {
    OID: '2.5.4.41',
    name: 'name',
  }, {
    OID: '2.5.4.42',
    name: 'givenName',
  }, {
    OID: '2.5.4.43',
    name: 'initials',
  }, {
    OID: '2.5.4.46',
    name: 'dnQualifier',
  }, {
    OID: '2.5.4.65',
    name: 'pseudonym',
  }, {
    OID: '1.2.840.113549.1.9.1',
    name: 'emailAddress',
  }, {
    OID: '1.2.840.113549.1.9.2',
    name: 'unstructuredName',
  }, {
    OID: '1.2.840.113549.1.9.8',
    name: 'unstructuredAddress',
  }, {
    OID: '0.9.2342.19200300.100.1.1',
    name: 'userid',
    shortName: 'UID',
  }, {
    OID: '0.9.2342.19200300.100.1.25',
    name: 'domainComponent',
    shortName: 'DC',
  },
];

/**
 * Applies the correct RegExp to the value according to the given attribute
 *
 * @param {string} attr Name of a field in SubjectDN
 * @param {string} value value of a field in SubjectDN
 */
function performRegexInAttrValue(attr, value) {
  if (Reflect.has(allowedRegex, attr)) {
    const regex = Reflect.get(allowedRegex, attr);
    if (regex instanceof RegExp && !regex.test(value)) {
      const errorMsg = `Error checking SubjectDN '${attr}' attribute in CSR. `
                  + `The value does not match the regular expression: ${regex.toString()}. `
                  + `Found: ${value}`;
      throw BadRequest(errorMsg);
    }
  }
}

/**
 * Checks whether the SubjectDN has only allowed fields, otherwise it does not pass validation.
 *
 * @param {Object} attributes SubjectDN fields
 */
function checkAllowedAttributes(attributes) {
  if (!attributes.every((attr) => Reflect.has(constantAttrs, attr) || allowed.includes(attr))) {
    const errorMsg = 'Error checking SubjectDN allowed attributes in CSR. '
            + `Allowed: [${allowed.toString()}] `
            + `Found: [${attributes.toString()}]`;
    throw BadRequest(errorMsg);
  }
}

/**
 * Checks whether the SubjectDN has the attributes considered mandatory.
 *
 * @param {Object} attributes SubjectDN fields
 */
function checkMandatoryAttributes(attributes) {
  if (!mandatory.every((mdAttr) => attributes.includes(mdAttr))) {
    const errorMsg = 'Error checking SubjectDN mandatory attributes in CSR. '
            + `Mandatory: [${mandatory.toString()}] `
            + `Found: [${attributes.toString()}]`;
    throw BadRequest(errorMsg);
  }
}

/**
 * Checks whether the values of the SubjectDN fields meet the input validation RegExp.
 *
 * @param {Object} attributes SubjectDN fields
 */
function checkAttributeValues(attributes) {
  attributes.forEach(({ attr, value }) => performRegexInAttrValue(attr, value));
}

/**
 * Performs checks on SubjectDN to ensure this meets the
 * rules of the platform before issuing the certificate.
 *
 * @throws exceptions if this does not pass the validations.
 */
function verify() {
  const attributes = Reflect.ownKeys(this).filter(
    ((attr) => typeof Reflect.get(this, attr) !== 'function'),
  );
  checkAllowedAttributes(attributes);
  checkMandatoryAttributes(attributes);
  checkAttributeValues(attributes.map(
    (attr) => ({ attr, value: Reflect.get(this, attr) }),
  ));
  return this;
}

/**
 * Converts SubjectDN fields to a string
 */
function stringify() {
  return Reflect.ownKeys(this).filter(
    ((attr) => typeof Reflect.get(this, attr) !== 'function'),
  ).map(
    ((attr) => [attr, Reflect.get(this, attr)].join('=')),
  ).join(', ');
}

/**
 * Adds a prefix to the SubjectDN Common Name field
 * @param {string} prefix CommonName (CN) prefix
 */
function cnamePrefix(prefix) {
  if (typeof prefix === 'string' && Reflect.has(this, 'CN')) {
    const cname = Reflect.get(this, 'CN');
    Reflect.set(this, 'CN', `${prefix}:${cname}`);
  }
  return this;
}

/**
 * Generates a DN object from a sequence of Relative Distinguished Names.
 * @param {pkijs.RelativeDistinguishedNames} relativeDNs - https://tools.ietf.org/html/rfc5280#section-4.1.2.4
 *                                                      or https://tools.ietf.org/html/rfc5280#section-4.1.2.6
 * @return {string} The Distinguished Name (DN object).
 */
function from(relativeDNs) {
  /* Returns the complete Distinguished Name (subjectDN|issuerDN) */
  return relativeDNs.typesAndValues.reduce(
    (obj, attr) => {
      const rdn = RDNs.find((el) => el.OID === attr.type);
      const key = (rdn) ? rdn.shortName || rdn.name : attr.type;
      if (!Reflect.has(obj, key)) {
        Reflect.set(obj, key, attr.value.valueBlock.value);
      }
      return obj;
    },
    {
      ...constantAttrs,
      verify,
      stringify,
      cnamePrefix,
    },
  );
}

/**
 * The subject DN is defined in RFC 2459 by the X.501 type Name as an ASN.1 structure.
 * It consists of a sequence of Relative Distinguished Names (RDNs), which are themselves
 * sets of attribute type and value pairs.
 */
module.exports = {
  from,
};
