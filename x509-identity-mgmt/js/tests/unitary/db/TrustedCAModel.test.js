const TrustedCAModel = require('../../../src/db/TrustedCAModel');

describe("Unit tests of script 'TrustedCAModel.js'", () => {
  let trustedCAModel = null;
  let db = null;

  beforeAll(() => {
    db = {
      parseProjectionFields: jest.fn(),
      sanitizeFields: jest.fn(),
    };
    trustedCAModel = new TrustedCAModel({ db });
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  it('should parse condition fields', () => {
    const d = new Date();
    const conditionFields = {
      caFingerprint: '$AB:CD',
      createdAt: d,
    };
    const result = trustedCAModel.parseConditionFields(conditionFields);
    expect(result).toEqual({
      caFingerprint: {
        $options: 'i',
        $regex: 'AB:CD$',
      },
      createdAt: d.toString(),
    });
  });

  it('should parse projection fields', () => {
    const commaSeparatedFields = 'validity,subjectDN';
    trustedCAModel.parseProjectionFields(commaSeparatedFields);
    expect(db.parseProjectionFields).toHaveBeenCalledTimes(1);
    expect(db.parseProjectionFields.mock.calls[0][0]).toBe(commaSeparatedFields);
  });

  it('this should sanitize (in depth) object attributes', () => {
    const cert = {
      caFingerprint: null,
      caPem: null,
    };
    trustedCAModel.sanitizeFields(cert);
    expect(db.sanitizeFields).toHaveBeenCalledTimes(1);
    expect(db.sanitizeFields.mock.calls[0][0]).toEqual(cert);
  });

  it('should throw an exception because it is a invalid conditional field', () => {
    const conditionFields = {
      caFingerprint: { toString: null },
    };
    expect(() => {
      trustedCAModel.parseConditionFields(conditionFields);
    }).toThrow();
  });
});
