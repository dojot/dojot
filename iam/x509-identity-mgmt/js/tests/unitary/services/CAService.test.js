const CAService = require('../../../src/services/CAService');

const util = require('../../util.test');

let containerCradleTemplate = null;

function pkiUtilsMock() {
  const pkiUtils = {
    parseCSR: jest.fn(() => ({ subjectPublicKeyInfo: {} })),
    checkPublicKey: jest.fn(),
    parseCert: jest.fn(() => ({
      notBefore: {
        value: new Date(),
      },
      notAfter: {
        value: new Date(),
      },
    })),
    getFingerprint: jest.fn(),
    checkRemainingDays: jest.fn(),
    assertLeaf: jest.fn(),
    isRootCA: jest.fn(),
    checkChainOfTrust: jest.fn(),
    getSerialNumber: jest.fn(),
  };
  return pkiUtils;
}

function ejbcaFacadeMock() {
  const ejbcaFacade = {
    generateCertificate: jest.fn(),
    revokeCertificate: jest.fn(),
  };
  return ejbcaFacade;
}

beforeAll(() => {
  containerCradleTemplate = {
    rootCA: global.config.ejbca.rootca,
  };
});

describe("Unit tests of script 'CAService.js'", () => {
  let containerCradle = null;

  beforeEach(() => {
    containerCradle = { ...containerCradleTemplate };

    containerCradle.ejbcaFacade = ejbcaFacadeMock();
    containerCradle.ejbcaFacade.getRootCertificate = jest.fn(() => util.caCert);
    containerCradle.ejbcaFacade.getCRL = jest.fn(() => util.caCRL);

    containerCradle.pkiUtils = pkiUtilsMock();
    containerCradle.pkiUtils.getFingerprint = jest.fn(() => util.caFingerprint);
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  it('should obtain the dojot root CA certificate', async () => {
    const caService = new CAService(containerCradle);

    await expect(caService.getRootCertificate())
      .resolves.toEqual({
        certificateFingerprint: util.caFingerprint,
        caPem: util.caCert,
      });

    expect(containerCradle.ejbcaFacade.getRootCertificate).toHaveBeenCalledTimes(1);
    expect(containerCradle.pkiUtils.getFingerprint).toHaveBeenCalledTimes(1);
  });

  it('should obtain the latest valid Certificate Revocation List', async () => {
    const caService = new CAService(containerCradle);

    await expect(caService.getRootCRL())
      .resolves.toEqual({
        crl: util.caCRL,
      });

    expect(containerCradle.ejbcaFacade.getCRL).toHaveBeenCalledTimes(1);
  });
});
