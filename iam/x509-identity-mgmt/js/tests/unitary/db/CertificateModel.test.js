const CertificateModel = require('../../../src/db/CertificateModel');

describe("Unit tests of script 'CertificateModel.js'", () => {
  let certificateModel = null;
  let mongoClient = null;

  beforeAll(() => {
    mongoClient = {
      parseProjectionFields: jest.fn(),
      sanitizeFields: jest.fn(),
    };
    certificateModel = new CertificateModel({ mongoClient });
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  it('should parse condition fields', () => {
    const d = new Date();
    const conditionFields = {
      fingerprint: '^AB:CD',
      caFingerprint: '$AB:CD',
      createdAt: d,
    };
    const result = certificateModel.parseConditionFields(conditionFields);
    expect(result).toEqual({ ...conditionFields, createdAt: d.toString() });
  });

  it('should parse param keyVal', () => {
    const conditionFilterFields = "belongsTo.device=null,belongsTo.application=!";
    const result = certificateModel.handleFilterField(conditionFilterFields);
    const resultNull = certificateModel.handleFilterField(null);
    expect(result).toEqual({
      "belongsTo.device": { '$exists': false },
      "belongsTo.application": { '$exists': false },
    });
    expect(resultNull).toEqual({});
  });

  it('should parse projection fields', () => {
    const commaSeparatedFields = 'validity,belongsTo';
    certificateModel.parseProjectionFields(commaSeparatedFields);
    expect(mongoClient.parseProjectionFields).toHaveBeenCalledTimes(1);
    expect(mongoClient.parseProjectionFields.mock.calls[0][0]).toBe(commaSeparatedFields);
  });

  it('this should sanitize (in depth) object attributes', () => {
    const cert = {
      fingerprint: null,
      pem: null,
    };
    certificateModel.sanitizeFields(cert);
    expect(mongoClient.sanitizeFields).toHaveBeenCalledTimes(1);
    expect(mongoClient.sanitizeFields.mock.calls[0][0]).toEqual(cert);
  });

  it('should throw an exception because it is a invalid conditional field', () => {
    const conditionFields = {
      fingerprint: { toString: null },
    };
    expect(() => {
      certificateModel.parseConditionFields(conditionFields);
    }).toThrow();
  });

  it('should parse sortBy fields', () => {
    const sortByCreatedAt = 'createdAt';
    const sortByCreatedAtAsc = 'asc:createdAt';
    const sortByCreatedAtDesc = 'desc:createdAt';
    
    expect(certificateModel.parseSortBy(sortByCreatedAt)).toEqual({ 
      createdAt: 'asc',
    });

    expect(certificateModel.parseSortBy(sortByCreatedAtAsc)).toEqual({ 
      createdAt: 'asc',
    });

    expect(certificateModel.parseSortBy(sortByCreatedAtDesc)).toEqual({ 
      createdAt: 'desc',
    });
  });
  
  it('should return null if cannot parse sortBy', () => {
    const sortBy = 'field-that-does-not-exist';
    expect(certificateModel.parseSortBy(sortBy)).toBe(null);
    expect(certificateModel.parseSortBy(123456)).toBe(null);
  });
});
