const createDnUtils = require('../../../src/core/dnUtils');

const { errorTemplate } = global;

describe("Unit tests of script 'dnUtils.js'", () => {
  describe('creation of an object where all its methods are executed without any problem', () => {
    let dnUtils = null;
    let config = null;
    let relativeDNs = null;

    beforeAll(() => {
      config = {
        allowedAttrs: ['CN'],
        allowedAttrsConstraints: ['CN=^[0-9A-Za-z .]{1,255}$'],
        mandatoryAttrs: ['CN'],
        constantAttrs: {
          O: 'dojot IoT Platform',
        },
      };
      dnUtils = createDnUtils({ config, errorTemplate });
      relativeDNs = {
        typesAndValues: [
          {
            type: '2.5.4.3', // CN - CommonName
            value: {
              valueBlock: {
                value: 'www.host.com',
              },
            },
          },
        ],
      };
    });

    it('should include the constant attributes', () => {
      const subjectDN = dnUtils.from(relativeDNs, true);
      expect(subjectDN).toMatchObject({
        CN: relativeDNs.typesAndValues[0].value.valueBlock.value,
        ...config.constantAttrs,
      });
      expect(subjectDN.verify()).toBeTruthy();
    });

    it('should include the constant attributes and overwrite the existing one', () => {
      const anotherDNs = { typesAndValues: [...relativeDNs.typesAndValues] };
      anotherDNs.typesAndValues.push({
        type: '2.5.4.10', // O - organizationName
        value: {
          valueBlock: {
            value: 'organization name to be overwrittenwww.host.com',
          },
        },
      });
      const subjectDN = dnUtils.from(anotherDNs, true);
      expect(subjectDN).toMatchObject({
        CN: relativeDNs.typesAndValues[0].value.valueBlock.value,
        ...config.constantAttrs,
      });
      expect(subjectDN.verify()).toBeTruthy();
    });

    it('should not include the constant attributes', () => {
      const subjectDN = dnUtils.from(relativeDNs);
      expect(subjectDN).toMatchObject({
        CN: relativeDNs.typesAndValues[0].value.valueBlock.value,
      });
      expect(subjectDN.verify()).toBeTruthy();
    });

    it('should converts SubjectDN fields to a string', () => {
      const subjectDN = dnUtils.from(relativeDNs);
      expect(subjectDN.stringify()).toBe('CN=www.host.com');
    });

    it('should adds a prefix to the SubjectDN Common Name field', () => {
      const prefix = 'custom-prefix';
      const result = `${prefix}:www.host.com`;
      const subjectDN = dnUtils.from(relativeDNs);
      expect(subjectDN.cnamePrefix(prefix)).toEqual({ CN: result });
    });

    it('should not add the prefix as it is not a string', () => {
      const prefix = 12345;
      const result = 'www.host.com';
      const subjectDN = dnUtils.from(relativeDNs);
      expect(subjectDN.cnamePrefix(prefix)).toEqual({ CN: result });
    });

    it('should ignore duplicate checking SubjectDN attribute values', () => {
      const configAlt = { ...config };
      configAlt.allowedAttrsConstraints = [
        'CN=^[0-9A-Za-z .]{1,255}$',
        'CN=^[0-9]{1,9}$', // duplicate attribute validation will be ignored...
      ];
      const dnUtilsAlt = createDnUtils({ config: configAlt, errorTemplate });
      const subjectDN = dnUtilsAlt.from(relativeDNs);
      expect(subjectDN).toMatchObject({
        CN: relativeDNs.typesAndValues[0].value.valueBlock.value,
      });
      expect(subjectDN.verify()).toBeTruthy();
    });
  });

  describe('SubjectDN does not pass validations', () => {
    let dnUtils = null;
    let config = null;

    beforeAll(() => {
      config = {
        allowedAttrs: ['CN', 'emailAddress'],
        allowedAttrsConstraints: ['CN=^[A-Za-z ]{1,255}$'],
        mandatoryAttrs: ['CN'],
        constantAttrs: {},
      };
      dnUtils = createDnUtils({ config, errorTemplate });
    });

    it('should throw an Error checking SubjectDN allowed attributes', () => {
      const relativeDNs = {
        typesAndValues: [
          {
            type: '0.0.0.0', // invalid OID
            value: {
              valueBlock: {
                value: 'any OID value',
              },
            },
          },
          {
            type: 'verify', // an invalid value that has the same name as a method of the SubjectDN object
            value: {
              valueBlock: {
                value: 'anything here',
              },
            },
          },
        ],
      };
      const subjectDN = dnUtils.from(relativeDNs, true);
      expect(() => {
        subjectDN.verify();
      }).toThrow();
    });

    it('should throw an Error checking SubjectDN mandatory attributes', () => {
      const relativeDNs = {
        typesAndValues: [
          {
            type: '1.2.840.113549.1.9.1', // emailAddress
            value: {
              valueBlock: {
                value: 'test@host.corp',
              },
            },
          },
        ],
      };
      const subjectDN = dnUtils.from(relativeDNs, true);
      expect(() => {
        subjectDN.verify();
      }).toThrow();
    });

    it('should throw an Error checking SubjectDN attribute values', () => {
      const relativeDNs = {
        typesAndValues: [
          {
            type: '2.5.4.3', // CN - CommonName
            value: {
              valueBlock: {
                value: 'a v@lue with inva1id charact&rs',
              },
            },
          },
        ],
      };
      const subjectDN = dnUtils.from(relativeDNs, true);
      expect(() => {
        subjectDN.verify();
      }).toThrow();
    });
  });
});
