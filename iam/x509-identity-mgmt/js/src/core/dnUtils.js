/* A distinguished name for an X.509 certificate consists
 * of a sequence of relative distinguished names (RDN).
 * For details on these SubjectDN attributes,
 * simply search the OID Repository:
 * http://oid-info.com/get/<OID-dot-notation>
 *
 * These are the Subject RDNs supported by the EJBCA.
 * For custom RDNs, you must also configure them in the EJBCA. */
const { catalog: RelativeDistinguishedNamesCatalog } = require('./RelativeDistinguishedNames.json');

/* parse regex validator for values in SubjectDN allowed attributes */
const parseAllowedAttrsRegex = (arr) => arr.reduce((obj, attr) => {
  const keyValue = attr.split('=').map((el) => el.trim());
  const key = keyValue[0];
  // eslint-disable-next-line security/detect-non-literal-regexp, security-node/non-literal-reg-expr
  const value = new RegExp(keyValue[1]);
  if (!Reflect.has(obj, key)) {
    Reflect.set(obj, key, value);
  }
  return obj;
}, {});

/**
 * The factory function pattern is similar to constructors, but instead of using
 * 'new' to create an object, factory functions simply set up and return the new
 * object when you call the function.
 *
 * @param {object} config Settings for creating a new object.
 * @param {object} errorTemplate to throw errors with HTTP code.
 *
 * @returns a factory function
 */
function createObject(config, errorTemplate) {
  const { BadRequest } = errorTemplate;

  /* List of allowed attributes on SubjectDN */
  const allowed = [...config.allowedAttrs];

  /* constraints on the values of the allowed attributes on SubjectDN */
  const allowedRegex = parseAllowedAttrsRegex(config.allowedAttrsConstraints);

  /* List of mandatory attributes on SubjectDN */
  const mandatory = [...config.mandatoryAttrs];

  /* list of attributes to be overwritten in SubjectDN */
  const { constantAttrs } = config;

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
    const attributes = Object.keys(this).filter(
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
    return Object.keys(this).filter(
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
   * @param {boolean} includeConstantAttrs include attributes to be overwritten in SubjectDN
   * @return {string} The Distinguished Name (DN object).
   */
  function from(relativeDNs, includeConstantAttrs = false) {
    let $ = {};

    // defines the object's methods using the default values for the Property Descriptor
    // therefore, the methods will NOT be enumerable, configurable, or writable
    Reflect.defineProperty($, 'verify', { value: verify });
    Reflect.defineProperty($, 'stringify', { value: stringify });
    Reflect.defineProperty($, 'cnamePrefix', { value: cnamePrefix });

    // reduce the RelativeDNs to a complete DistinguishedName (subjectDN|issuerDN)
    // and yet maintaining the order of the RDNs (through enumerable properties)
    $ = relativeDNs.typesAndValues.reduce(
      (obj, attr) => {
        const rdn = RelativeDistinguishedNamesCatalog.find((el) => el.OID === attr.type);
        const key = (rdn) ? (rdn.shortName || rdn.name) : attr.type;
        if (!Reflect.has(obj, key)) {
          // defines the object's properties as enumerable and writable
          Reflect.defineProperty(obj, key, {
            writable: true,
            enumerable: true,
            value: attr.value.valueBlock.value,
          });
        }
        return obj;
      }, $,
    );

    if (includeConstantAttrs) {
      // it is important to keep the same Property Descriptor
      // configuration used for previous RelativeDNs...
      Object.entries(constantAttrs).forEach(
        ([key, value]) => (
          (Reflect.has($, key))
            ? Reflect.set($, key, value)
            : Reflect.defineProperty($, key, {
              writable: true,
              enumerable: true,
              value,
            })
        ),
      );
    }
    return $;
  }

  return Reflect.construct(function DNUtils() {
    this.from = from;
  }, []);
}

/**
 * The subject DN is defined in RFC 2459 by the X.501 type Name as an ASN.1 structure.
 * It consists of a sequence of Relative Distinguished Names (RDNs), which are themselves
 * sets of attribute type and value pairs.
 */
module.exports = ({ config, errorTemplate }) => createObject(config, errorTemplate);
