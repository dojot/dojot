const createSchemaValidator = require('../../../src/core/schemaValidator');
const errorTemplate = require('../../../src/sdk/web/framework/backing/error-template');
const utils = require('../../util.test');

const defsSchema = require('../../../schemas/defs.json');
const regTrustCaSchema = require('../../../schemas/register-trusted-ca-certificate.json');
const updTrustCaSchema = require('../../../schemas/update-trusted-ca-certificate.json');
const regOrGenCertSchema = require('../../../schemas/register-or-generate-certificate.json');
const chOwnCertSchema = require('../../../schemas/change-owner-certificate.json');

describe("Unit tests of script 'schemaValidator.js'", () => {
  let schemas = null;
  let schemaValidator = null;

  beforeAll(() => {
    schemas = {
      defs: defsSchema,
      regTrustCa: regTrustCaSchema,
      updTrustCa: updTrustCaSchema,
      regOrGenCert: regOrGenCertSchema,
      chOwnCert: chOwnCertSchema,
    };
    schemaValidator = createSchemaValidator({ schemas, errorTemplate });
  });

  it('should validate new trusted CAs', () => {
    const validationMiddleware = schemaValidator.validateNewTrustedCA();
    const req = {
      body: {
        caPem: utils.caCert,
      },
    };
    const res = {};
    const next = jest.fn();

    validationMiddleware(req, res, next);

    expect(next).toHaveBeenCalledTimes(1);
    expect(next.mock.calls[0][0]).toBeUndefined();
  });

  it('should validate the AutoRegistration update of the trusted CA', () => {
    const validationMiddleware = schemaValidator.validateUpdTrustedCA();
    const req = {
      body: {
        allowAutoRegistration: true,
      },
    };
    const res = {};
    const next = jest.fn();

    validationMiddleware(req, res, next);

    expect(next).toHaveBeenCalledTimes(1);
    expect(next.mock.calls[0][0]).toBeUndefined();
  });

  it('should validate the registration of certificates', () => {
    const validationMiddleware = schemaValidator.validateRegOrGenCert();
    const req = {
      body: {
        caFingerprint: utils.certChainRootCAFingerprint,
        certificateChain: utils.certChain.join('\n').replace(/^(\s*)(.*)(\s*$)/gm, '$2'),
      },
    };
    const res = {};
    const next = jest.fn();

    validationMiddleware(req, res, next);

    expect(next).toHaveBeenCalledTimes(1);
    expect(next.mock.calls[0][0]).toBeUndefined();
  });

  it('should validate the generation of certificates', () => {
    const validationMiddleware = schemaValidator.validateRegOrGenCert();
    const req = {
      body: { csr: utils.p256CSR },
    };
    const res = {};
    const next = jest.fn();

    validationMiddleware(req, res, next);

    expect(next).toHaveBeenCalledTimes(1);
    expect(next.mock.calls[0][0]).toBeUndefined();
  });

  it('should validate the change of certificate owner (device)', () => {
    const validationMiddleware = schemaValidator.validateChangeOwnerCert();
    const req = {
      body: {
        belongsTo: {
          device: '1234567890',
        },
      },
    };
    const res = {};
    const next = jest.fn();

    validationMiddleware(req, res, next);

    expect(next).toHaveBeenCalledTimes(1);
    expect(next.mock.calls[0][0]).toBeUndefined();
  });

  it('should validate the change of certificate owner (application)', () => {
    const validationMiddleware = schemaValidator.validateChangeOwnerCert();
    const req = {
      body: {
        belongsTo: {
          application: 'kafka-consumer',
        },
      },
    };
    const res = {};
    const next = jest.fn();

    validationMiddleware(req, res, next);

    expect(next).toHaveBeenCalledTimes(1);
    expect(next.mock.calls[0][0]).toBeUndefined();
  });


  it('should throw an exception because it failed to validate a new trusted CA', () => {
    const validationMiddleware = schemaValidator.validateNewTrustedCA();
    const req = { body: {} };
    const res = {};
    const next = jest.fn();

    validationMiddleware(req, res, next);

    expect(next).toHaveBeenCalledTimes(1);
    expect(next.mock.calls[0][0]).toBeInstanceOf(Error);
  });

  it('should throw an exception because it failed to validate the AutoRegistration update of the trusted CA', () => {
    const validationMiddleware = schemaValidator.validateUpdTrustedCA();
    const req = { body: { } };
    const res = {};
    const next = jest.fn();

    validationMiddleware(req, res, next);

    expect(next).toHaveBeenCalledTimes(1);
    expect(next.mock.calls[0][0]).toBeInstanceOf(Error);
  });

  it('should throw an exception because it failed to validate the registration of certificates', () => {
    const validationMiddleware = schemaValidator.validateRegOrGenCert();
    const req = { body: { } };
    const res = {};
    const next = jest.fn();

    validationMiddleware(req, res, next);

    expect(next).toHaveBeenCalledTimes(1);
    expect(next.mock.calls[0][0]).toBeInstanceOf(Error);
  });

  it('should throw an exception because it failed to validate the generation of certificates', () => {
    const validationMiddleware = schemaValidator.validateRegOrGenCert();
    const req = { body: { } };
    const res = {};
    const next = jest.fn();

    validationMiddleware(req, res, next);

    expect(next).toHaveBeenCalledTimes(1);
    expect(next.mock.calls[0][0]).toBeInstanceOf(Error);
  });

  it('should throw an exception because it failed to validate the change of certificate owner (device)', () => {
    const validationMiddleware = schemaValidator.validateChangeOwnerCert();
    const req = { body: { } };
    const res = {};
    const next = jest.fn();

    validationMiddleware(req, res, next);

    expect(next).toHaveBeenCalledTimes(1);
    expect(next.mock.calls[0][0]).toBeInstanceOf(Error);
  });

  it('should throw an exception because it failed to validate the change of certificate owner (application)', () => {
    const validationMiddleware = schemaValidator.validateChangeOwnerCert();
    const req = { body: { } };
    const res = {};
    const next = jest.fn();

    validationMiddleware(req, res, next);

    expect(next).toHaveBeenCalledTimes(1);
    expect(next.mock.calls[0][0]).toBeInstanceOf(Error);
  });
});
