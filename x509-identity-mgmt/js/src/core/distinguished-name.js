const { certificate: certCfg } = require('../config');

const { BadRequest } = require('./errors');

/* List of allowed attributes on SubjectDN */
const allowed = [...certCfg.subject.allowedAttrs];

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
    identifier: 'O',
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

/* constraints on the values of the allowed attributes on SubjectDN */
const commonNameRegex = /^[0-9A-Za-z ]{1,255}$/;
const constraints = {
  CN(value) {
    if (!commonNameRegex.test(value)) {
      const errorMsg = 'Error checking SubjectDN CommonName (CN) attribute in CSR. '
                + `The value does not match the regular expression: ${commonNameRegex.toString()}. `
                + `Found: ${value}; `;
      throw BadRequest(errorMsg);
    }
  },
};

function checkAllowedAttributes(attributes) {
  if (!attributes.every((attr) => Reflect.has(constantAttrs, attr) || allowed.includes(attr))) {
    const errorMsg = 'Error checking SubjectDN allowed attributes in CSR. '
            + `Allowed: [${allowed.toString()}] `
            + `Found: [${attributes.toString()}] `;
    throw BadRequest(errorMsg);
  }
}

function checkMandatoryAttributes(attributes) {
  if (!mandatory.every((mdAttr) => attributes.includes(mdAttr))) {
    const errorMsg = 'Error checking SubjectDN mandatory attributes in CSR. '
            + `Mandatory: [${mandatory.toString()}] `
            + `Found: [${attributes.toString()}] `;
    throw BadRequest(errorMsg);
  }
}

function checkAttributeValues(attributes) {
  attributes.forEach(({ key, value }) => Reflect.has(constraints, key)
            && Reflect.apply(Reflect.get(constraints, key), constraints, [value]));
}

function verify() {
  const attributes = Reflect.ownKeys(this).filter(
    ((attr) => typeof Reflect.get(this, attr) !== 'function'), this,
  );
  checkAllowedAttributes(attributes);
  checkMandatoryAttributes(attributes);
  checkAttributeValues(attributes.map(
    (attr) => ({ key: attr, value: Reflect.get(this, attr) }),
  ));
  return this;
}

function stringify() {
  return Reflect.ownKeys(this).filter(
    ((attr) => typeof Reflect.get(this, attr) !== 'function'), this,
  ).map(
    ((attr) => [attr, Reflect.get(this, attr)].join('=')), this,
  ).join(', ');
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
