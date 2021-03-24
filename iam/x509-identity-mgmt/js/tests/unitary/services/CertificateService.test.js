const { Logger, WebUtils } = require('@dojot/microservice-sdk');

const CertificateService = require('../../../src/services/CertificateService');

const util = require('../../util.test');

let containerCradleTemplate = null;

function certificateModelMock() {
  const modelInstance = {
    save: jest.fn(() => ({
      belongsTo: {},
    })),
  };

  const certificateModel = {
    model: jest.fn().mockImplementation(() => modelInstance),
  };

  const modelRef = certificateModel.model;
  modelRef.instance = modelInstance;
  modelRef.find = jest.fn(() => modelRef);
  modelRef.findOne = jest.fn(() => modelRef);
  modelRef.findOneAndUpdate = jest.fn(() => modelRef);
  modelRef.findByIdAndDelete = jest.fn(() => modelRef);
  modelRef.select = jest.fn(() => modelRef);
  modelRef.limit = jest.fn(() => modelRef);
  modelRef.skip = jest.fn(() => modelRef);
  modelRef.maxTimeMS = jest.fn(() => modelRef);
  modelRef.lean = jest.fn(() => modelRef);
  modelRef.exec = jest.fn();
  modelRef.countDocuments = jest.fn();

  return certificateModel;
}

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

function dnUtilsMock() {
  const dn = {
    verify: jest.fn(() => dn),
    cnamePrefix: jest.fn(() => dn),
    stringify: jest.fn(),
  };

  const dnUtils = {
    from: jest.fn(() => dn),
  };
  dnUtils.from.dn = dn;

  return dnUtils;
}

function ejbcaFacadeMock() {
  const ejbcaFacade = {
    generateCertificate: jest.fn(),
    revokeCertificate: jest.fn(),
  };
  return ejbcaFacade;
}

function trustedCAServiceMock() {
  const trustedCAService = {
    getPEM: jest.fn(),
    registerCertificate: jest.fn(),
  };
  return trustedCAService;
}

function deviceMgrProviderMock() {
  const deviceMgrProvider = {
    checkDeviceExists: jest.fn().mockResolvedValue(true),
  };
  return deviceMgrProvider;
}

function ownershipNotifierMock() {
  const ownershipNotifier = {
    creation: jest.fn().mockResolvedValue(undefined),
    change: jest.fn().mockResolvedValue(undefined),
    removal: jest.fn().mockResolvedValue(undefined),
  };
  return ownershipNotifier;
}

beforeAll(() => {
  const certificateModel = certificateModelMock();

  const pkiUtils = pkiUtilsMock();

  const dnUtils = dnUtilsMock();

  const ejbcaFacade = ejbcaFacadeMock();

  const trustedCAService = trustedCAServiceMock();

  const deviceMgrProvider = deviceMgrProviderMock();

  const ownershipNotifier = ownershipNotifierMock();

  containerCradleTemplate = {
    deviceMgrProvider,
    certificateModel,
    trustedCAService,
    ejbcaFacade,
    ownershipNotifier,
    pkiUtils,
    dnUtils,
    tenant: 'admin',
    certValidity: 365,
    checkPublicKey: true,
    checkSubjectDN: true,
    checkDeviceExists: true,
    queryMaxTimeMS: global.config.mongo.query.maxtimems,
    certMinimumValidityDays: global.config.certificate.external.minimumvaliditydays,
    caCertAutoRegistration: global.config.certificate.external.ca.autoregistration,
    errorTemplate: WebUtils.framework.errorTemplate,
    logger: new Logger('CertificateService.test.js'),
  };
});

describe("Unit tests of script 'CertificateService.js'", () => {
  afterEach(() => {
    jest.clearAllMocks();
  });

  describe('certificate generation based on CSR', () => {
    let containerCradle = null;

    beforeEach(() => {
      containerCradle = { ...containerCradleTemplate };

      containerCradle.ejbcaFacade = ejbcaFacadeMock();
      containerCradle.ejbcaFacade.generateCertificate = jest.fn(() => util.p256Cert);

      containerCradle.pkiUtils = pkiUtilsMock();
      containerCradle.pkiUtils.getFingerprint = jest.fn(() => util.p256CertFingerprint);

      containerCradle.deviceMgrProvider = deviceMgrProviderMock();
    });

    it('should generate a certificate (with public key verification)', async () => {
      const certificateService = new CertificateService(containerCradle);

      await expect(certificateService.generateCertificate({
        csr: util.p256CSR,
      })).resolves.toEqual({
        certificateFingerprint: util.p256CertFingerprint,
        certificatePem: util.p256Cert,
      });

      expect(containerCradle.deviceMgrProvider.checkDeviceExists).toHaveBeenCalledTimes(1);

      expect(containerCradle.pkiUtils.parseCSR).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.checkPublicKey).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.parseCert).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.getFingerprint).toHaveBeenCalledTimes(1);

      expect(containerCradle.dnUtils.from).toHaveBeenCalledTimes(1);
      expect(containerCradle.dnUtils.from.dn.verify).toHaveBeenCalledTimes(1);
      expect(containerCradle.dnUtils.from.dn.cnamePrefix).toHaveBeenCalledTimes(1);
      expect(containerCradle.dnUtils.from.dn.stringify).toHaveBeenCalledTimes(1);

      expect(containerCradle.ejbcaFacade.generateCertificate).toHaveBeenCalledTimes(1);

      expect(containerCradle.certificateModel.model).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.instance.save).toHaveBeenCalledTimes(1);

      expect(containerCradle.ownershipNotifier.creation).toHaveBeenCalledTimes(1);
    });

    it('should generate a certificate (without public key verification)', async () => {
      containerCradle.checkPublicKey = false;
      const certificateService = new CertificateService(containerCradle);

      await expect(certificateService.generateCertificate({
        csr: util.p256CSR,
      })).resolves.toEqual({
        certificateFingerprint: util.p256CertFingerprint,
        certificatePem: util.p256Cert,
      });

      // without checking public key
      expect(containerCradle.pkiUtils.checkPublicKey).toHaveBeenCalledTimes(0);

      expect(containerCradle.deviceMgrProvider.checkDeviceExists).toHaveBeenCalledTimes(1);

      expect(containerCradle.pkiUtils.parseCSR).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.parseCert).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.getFingerprint).toHaveBeenCalledTimes(1);

      expect(containerCradle.dnUtils.from).toHaveBeenCalledTimes(1);
      expect(containerCradle.dnUtils.from.dn.verify).toHaveBeenCalledTimes(1);
      expect(containerCradle.dnUtils.from.dn.cnamePrefix).toHaveBeenCalledTimes(1);
      expect(containerCradle.dnUtils.from.dn.stringify).toHaveBeenCalledTimes(1);

      expect(containerCradle.ejbcaFacade.generateCertificate).toHaveBeenCalledTimes(1);

      expect(containerCradle.certificateModel.model).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.instance.save).toHaveBeenCalledTimes(1);

      expect(containerCradle.ownershipNotifier.creation).toHaveBeenCalledTimes(1);
    });

    it('should generate a certificate (without Subject DN verification)', async () => {
      containerCradle.checkSubjectDN = false;
      const certificateService = new CertificateService(containerCradle);

      await expect(certificateService.generateCertificate({
        csr: util.p256CSR,
      })).resolves.toEqual({
        certificateFingerprint: util.p256CertFingerprint,
        certificatePem: util.p256Cert,
      });

      expect(containerCradle.pkiUtils.checkPublicKey).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.parseCSR).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.parseCert).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.getFingerprint).toHaveBeenCalledTimes(1);

      expect(containerCradle.dnUtils.from).toHaveBeenCalledTimes(1);
      expect(containerCradle.dnUtils.from.dn.stringify).toHaveBeenCalledTimes(1);

      // without checking Subject DN
      expect(containerCradle.dnUtils.from.dn.verify).toHaveBeenCalledTimes(0);
      expect(containerCradle.dnUtils.from.dn.cnamePrefix).toHaveBeenCalledTimes(0);
      expect(containerCradle.deviceMgrProvider.checkDeviceExists).toHaveBeenCalledTimes(0);

      expect(containerCradle.ejbcaFacade.generateCertificate).toHaveBeenCalledTimes(1);

      expect(containerCradle.certificateModel.model).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.instance.save).toHaveBeenCalledTimes(1);

      expect(containerCradle.ownershipNotifier.creation).toHaveBeenCalledTimes(1);
    });

    it('should generate a certificate (without checking if the device exists)', async () => {
      containerCradle.checkDeviceExists = false;
      const certificateService = new CertificateService(containerCradle);

      await expect(certificateService.generateCertificate({
        csr: util.p256CSR,
      })).resolves.toEqual({
        certificateFingerprint: util.p256CertFingerprint,
        certificatePem: util.p256Cert,
      });

      expect(containerCradle.pkiUtils.checkPublicKey).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.parseCSR).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.parseCert).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.getFingerprint).toHaveBeenCalledTimes(1);

      expect(containerCradle.dnUtils.from).toHaveBeenCalledTimes(1);
      expect(containerCradle.dnUtils.from.dn.stringify).toHaveBeenCalledTimes(1);

      expect(containerCradle.dnUtils.from.dn.verify).toHaveBeenCalledTimes(1);
      expect(containerCradle.dnUtils.from.dn.cnamePrefix).toHaveBeenCalledTimes(1);

      // without checking if the device exists
      expect(containerCradle.deviceMgrProvider.checkDeviceExists).toHaveBeenCalledTimes(0);

      expect(containerCradle.ejbcaFacade.generateCertificate).toHaveBeenCalledTimes(1);

      expect(containerCradle.certificateModel.model).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.instance.save).toHaveBeenCalledTimes(1);

      expect(containerCradle.ownershipNotifier.creation).toHaveBeenCalledTimes(1);
    });

    it('should generate a certificate (with device association)', async () => {
      const certificateService = new CertificateService(containerCradle);

      await expect(certificateService.generateCertificate({
        csr: util.p256CSR, belongsTo: { device: 'abc123' },
      })).resolves.toEqual({
        certificateFingerprint: util.p256CertFingerprint,
        certificatePem: util.p256Cert,
      });

      // the first call validates the device ID in the CSR's CN
      // the second validates the device association payload
      expect(containerCradle.deviceMgrProvider.checkDeviceExists).toHaveBeenCalledTimes(2);

      expect(containerCradle.pkiUtils.parseCSR).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.checkPublicKey).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.parseCert).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.getFingerprint).toHaveBeenCalledTimes(1);

      expect(containerCradle.dnUtils.from).toHaveBeenCalledTimes(1);
      expect(containerCradle.dnUtils.from.dn.verify).toHaveBeenCalledTimes(1);
      expect(containerCradle.dnUtils.from.dn.cnamePrefix).toHaveBeenCalledTimes(1);
      expect(containerCradle.dnUtils.from.dn.stringify).toHaveBeenCalledTimes(1);

      expect(containerCradle.ejbcaFacade.generateCertificate).toHaveBeenCalledTimes(1);

      expect(containerCradle.certificateModel.model).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.instance.save).toHaveBeenCalledTimes(1);

      expect(containerCradle.ownershipNotifier.creation).toHaveBeenCalledTimes(1);
    });

    it('should throw an exception for not finding the device for the informed tenant', async () => {
      // This mock simulates that the informed device has no relation to the tenant
      // the first call validates the device ID in the CSR's CN
      // the second does not validate the device association payload
      containerCradle.deviceMgrProvider.checkDeviceExists = jest.fn()
        .mockResolvedValueOnce(true)
        .mockResolvedValue(false);

      const certificateService = new CertificateService(containerCradle);

      await expect(certificateService.generateCertificate({
        csr: util.p256CSR, belongsTo: { device: 'abc123' },
      })).rejects.toThrow();

      expect(containerCradle.deviceMgrProvider.checkDeviceExists).toHaveBeenCalledTimes(2);

      expect(containerCradle.pkiUtils.parseCSR).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.checkPublicKey).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.parseCert).toHaveBeenCalledTimes(0);
      expect(containerCradle.pkiUtils.getFingerprint).toHaveBeenCalledTimes(0);

      expect(containerCradle.dnUtils.from).toHaveBeenCalledTimes(1);
      expect(containerCradle.dnUtils.from.dn.verify).toHaveBeenCalledTimes(1);
      expect(containerCradle.dnUtils.from.dn.cnamePrefix).toHaveBeenCalledTimes(1);
      expect(containerCradle.dnUtils.from.dn.stringify).toHaveBeenCalledTimes(1);

      expect(containerCradle.ejbcaFacade.generateCertificate).toHaveBeenCalledTimes(0);

      expect(containerCradle.certificateModel.model).toHaveBeenCalledTimes(0);
      expect(containerCradle.certificateModel.model.instance.save).toHaveBeenCalledTimes(0);

      expect(containerCradle.ownershipNotifier.creation).toHaveBeenCalledTimes(0);
    });
  });

  describe('external certificate registration', () => {
    let containerCradle = null;

    beforeEach(() => {
      containerCradle = { ...containerCradleTemplate };
      containerCradle.pkiUtils = pkiUtilsMock();
      containerCradle.certificateModel = certificateModelMock();
      containerCradle.trustedCAService = trustedCAServiceMock();

      containerCradle.certificateModel.model.countDocuments = jest.fn(() => 0);
      containerCradle.deviceMgrProvider = deviceMgrProviderMock();
    });

    it('should register an external certificate', async () => {
      containerCradle.pkiUtils.isRootCA = jest.fn().mockResolvedValue(true);

      containerCradle.pkiUtils.getFingerprint = jest.fn()
        .mockImplementationOnce(() => util.certChainHostFingerprint)
        .mockImplementationOnce(() => util.certChainRootCAFingerprint);

      containerCradle.trustedCAService.getPEM = jest.fn(
        () => util.certChain[util.certChain.length - 1],
      );

      const certificateService = new CertificateService(containerCradle);

      await expect(certificateService.registerCertificate({
        caFingerprint: util.certChainRootCAFingerprint,
        certificateChain: util.certChain,
      })).resolves.toEqual({
        certificateFingerprint: util.certChainHostFingerprint,
      });

      expect(containerCradle.deviceMgrProvider.checkDeviceExists).toHaveBeenCalledTimes(0);
      expect(containerCradle.pkiUtils.parseCert).toHaveBeenCalledTimes(5);
      expect(containerCradle.pkiUtils.getFingerprint).toHaveBeenCalledTimes(2);
      expect(containerCradle.pkiUtils.checkRemainingDays).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.assertLeaf).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.isRootCA).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.countDocuments).toHaveBeenCalledTimes(1);

      expect(containerCradle.ownershipNotifier.creation).toHaveBeenCalledTimes(1);
    });

    it('should register an external certificate by the fingerprint of the root CA', async () => {
      containerCradle.pkiUtils.isRootCA = jest.fn().mockResolvedValue(false);

      containerCradle.pkiUtils.getFingerprint = jest.fn()
        .mockImplementationOnce(() => util.certChainHostFingerprint);

      containerCradle.trustedCAService.getPEM = jest.fn(
        () => util.certChain[util.certChain.length - 1],
      );

      const certificateService = new CertificateService(containerCradle);

      const [hostCert, intermediateCA] = util.certChain;

      await expect(certificateService.registerCertificate({
        caFingerprint: util.certChainRootCAFingerprint,
        certificateChain: [hostCert, intermediateCA],
      })).resolves.toEqual({
        certificateFingerprint: util.certChainHostFingerprint,
      });

      expect(containerCradle.pkiUtils.parseCert).toHaveBeenCalledTimes(5);
      expect(containerCradle.pkiUtils.getFingerprint).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.checkRemainingDays).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.assertLeaf).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.isRootCA).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.countDocuments).toHaveBeenCalledTimes(1);

      expect(containerCradle.ownershipNotifier.creation).toHaveBeenCalledTimes(1);
    });

    it('should register the external certificate and also your CA', async () => {
      containerCradle.pkiUtils.isRootCA = jest.fn().mockResolvedValue(true);

      containerCradle.pkiUtils.getFingerprint = jest.fn()
        .mockImplementationOnce(() => util.certChainHostFingerprint)
        .mockImplementationOnce(() => util.certChainRootCAFingerprint);

      containerCradle.trustedCAService.getPEM = jest.fn(() => {
        throw new Error();
      });

      containerCradle.caCertAutoRegistration = true;

      const certificateService = new CertificateService(containerCradle);

      await expect(certificateService.registerCertificate({
        certificateChain: util.certChain,
      })).resolves.toEqual({
        certificateFingerprint: util.certChainHostFingerprint,
      });

      expect(containerCradle.pkiUtils.parseCert).toHaveBeenCalledTimes(5);
      expect(containerCradle.pkiUtils.getFingerprint).toHaveBeenCalledTimes(2);
      expect(containerCradle.pkiUtils.checkRemainingDays).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.assertLeaf).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.isRootCA).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.countDocuments).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAService.registerCertificate).toHaveBeenCalledTimes(1);

      expect(containerCradle.ownershipNotifier.creation).toHaveBeenCalledTimes(1);
    });

    it('should throw an exception because the fingerprint already exists', async () => {
      containerCradle.pkiUtils.getFingerprint = jest.fn()
        .mockImplementationOnce(() => util.certChainHostFingerprint);

      containerCradle.certificateModel.model.countDocuments = jest.fn(() => 1);

      const certificateService = new CertificateService(containerCradle);

      await expect(certificateService.registerCertificate({
        caFingerprint: util.certChainRootCAFingerprint,
        certificateChain: util.certChain,
      })).rejects.toThrow();

      expect(containerCradle.ownershipNotifier.creation).toHaveBeenCalledTimes(0);
    });

    it('should throw an exception because the root CA fingerprint was not provided', async () => {
      containerCradle.pkiUtils.getFingerprint = jest.fn()
        .mockImplementationOnce(() => util.certChainHostFingerprint);

      const certificateService = new CertificateService(containerCradle);

      await expect(certificateService.registerCertificate({
        certificateChain: [util.certChain[util.certChain.length - 1]],
      })).rejects.toThrow();

      expect(containerCradle.ownershipNotifier.creation).toHaveBeenCalledTimes(0);
    });

    it('should throw an error because the CA fingerprint is different from the one calculated on the certificate', async () => {
      containerCradle.pkiUtils.isRootCA = jest.fn().mockResolvedValue(true);

      containerCradle.pkiUtils.getFingerprint = jest.fn()
        .mockImplementationOnce(() => util.certChainHostFingerprint)
        .mockImplementationOnce(() => util.certChainRootCAFingerprint);

      const certificateService = new CertificateService(containerCradle);

      await expect(certificateService.registerCertificate({
        caFingerprint: 'ABC', // informed fingerprint does not match the one calculated using the CA certificate
        certificateChain: util.certChain,
      })).rejects.toThrow();

      expect(containerCradle.ownershipNotifier.creation).toHaveBeenCalledTimes(0);
    });

    it('should throw an exception because the root CA certificate has not been previously registered', async () => {
      containerCradle.pkiUtils.isRootCA = jest.fn().mockResolvedValue(true);

      containerCradle.pkiUtils.getFingerprint = jest.fn()
        .mockImplementationOnce(() => util.certChainHostFingerprint)
        .mockImplementationOnce(() => util.certChainRootCAFingerprint);

      containerCradle.trustedCAService.getPEM = jest.fn(() => {
        throw new Error();
      });

      const certificateService = new CertificateService(containerCradle);

      await expect(certificateService.registerCertificate({
        certificateChain: util.certChain,
      })).rejects.toThrow();

      expect(containerCradle.ownershipNotifier.creation).toHaveBeenCalledTimes(0);
    });

    it('should throw an exception for not finding the device for the informed tenant', async () => {
      // This mock simulates that the informed device has no relation to the tenant
      containerCradle.deviceMgrProvider.checkDeviceExists = jest.fn().mockResolvedValue(false);

      containerCradle.pkiUtils.isRootCA = jest.fn().mockResolvedValue(true);

      containerCradle.pkiUtils.getFingerprint = jest.fn()
        .mockImplementationOnce(() => util.certChainHostFingerprint)
        .mockImplementationOnce(() => util.certChainRootCAFingerprint);

      containerCradle.trustedCAService.getPEM = jest.fn(
        () => util.certChain[util.certChain.length - 1],
      );

      const certificateService = new CertificateService(containerCradle);

      await expect(certificateService.registerCertificate({
        caFingerprint: util.certChainRootCAFingerprint,
        certificateChain: util.certChain,
        belongsTo: { device: 'abc123' },
      })).rejects.toThrow();

      expect(containerCradle.deviceMgrProvider.checkDeviceExists).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.parseCert).toHaveBeenCalledTimes(0);
      expect(containerCradle.pkiUtils.getFingerprint).toHaveBeenCalledTimes(0);
      expect(containerCradle.pkiUtils.checkRemainingDays).toHaveBeenCalledTimes(0);
      expect(containerCradle.pkiUtils.assertLeaf).toHaveBeenCalledTimes(0);
      expect(containerCradle.pkiUtils.isRootCA).toHaveBeenCalledTimes(0);
      expect(containerCradle.certificateModel.model.countDocuments).toHaveBeenCalledTimes(0);

      expect(containerCradle.ownershipNotifier.creation).toHaveBeenCalledTimes(0);
    });
  });

  describe('ownership change associated with the certificate', () => {
    let containerCradle = null;

    beforeEach(() => {
      containerCradle = { ...containerCradleTemplate };
      containerCradle.certificateModel = certificateModelMock();
    });

    it("should change the ownership associated with the certificate ('null' belongsTo, nothing changes)", async () => {
      const oldBelongsTo = {};
      const newBelongsTo = {};

      // returns the found document (mock)
      containerCradle.certificateModel.model.exec = jest.fn(() => ({
        belongsTo: oldBelongsTo,
      }));

      const certificateService = new CertificateService(containerCradle);

      await expect(certificateService.changeOwnership({}, newBelongsTo)).resolves.toBeUndefined();

      expect(containerCradle.certificateModel.model.findOneAndUpdate).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.maxTimeMS).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.exec).toHaveBeenCalledTimes(1);

      expect(containerCradle.ownershipNotifier.creation).toHaveBeenCalledTimes(0);
      expect(containerCradle.ownershipNotifier.change).toHaveBeenCalledTimes(0);
      expect(containerCradle.ownershipNotifier.removal).toHaveBeenCalledTimes(0);
    });

    it('should change the ownership associated with the certificate (owner = device)', async () => {
      const oldBelongsTo = { device: 'abc123' };
      const newBelongsTo = { device: '123abc' };

      // returns the found document (mock)
      containerCradle.certificateModel.model.exec = jest.fn(() => ({
        belongsTo: oldBelongsTo,
      }));

      const certificateService = new CertificateService(containerCradle);

      await expect(certificateService.changeOwnership({}, newBelongsTo)).resolves.toBeUndefined();

      expect(containerCradle.certificateModel.model.findOneAndUpdate).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.maxTimeMS).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.exec).toHaveBeenCalledTimes(1);

      expect(containerCradle.ownershipNotifier.change).toHaveBeenCalledTimes(1);
    });

    it('should change the ownership associated with the certificate (owner = application)', async () => {
      const oldBelongsTo = { application: 'v2k' };
      const newBelongsTo = { application: 'k2v' };

      // returns the found document (mock)
      containerCradle.certificateModel.model.exec = jest.fn(() => ({
        belongsTo: oldBelongsTo,
      }));

      const certificateService = new CertificateService(containerCradle);

      await expect(certificateService.changeOwnership({}, newBelongsTo)).resolves.toBeUndefined();

      expect(containerCradle.certificateModel.model.findOneAndUpdate).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.maxTimeMS).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.exec).toHaveBeenCalledTimes(1);

      expect(containerCradle.ownershipNotifier.change).toHaveBeenCalledTimes(1);
    });

    it('should change (create) the ownership associated with the certificate (owner = device)', async () => {
      const oldBelongsTo = { device: null };
      const newBelongsTo = { device: '123abc' };

      // returns the found document (mock)
      containerCradle.certificateModel.model.exec = jest.fn(() => ({
        belongsTo: oldBelongsTo,
      }));

      const certificateService = new CertificateService(containerCradle);

      await expect(certificateService.changeOwnership({}, newBelongsTo)).resolves.toBeUndefined();

      expect(containerCradle.certificateModel.model.findOneAndUpdate).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.maxTimeMS).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.exec).toHaveBeenCalledTimes(1);

      expect(containerCradle.ownershipNotifier.creation).toHaveBeenCalledTimes(1);
    });

    it('should change (create) the ownership associated with the certificate (owner = application)', async () => {
      const oldBelongsTo = { application: null };
      const newBelongsTo = { application: 'k2v' };

      // returns the found document (mock)
      containerCradle.certificateModel.model.exec = jest.fn(() => ({
        belongsTo: oldBelongsTo,
      }));

      const certificateService = new CertificateService(containerCradle);

      await expect(certificateService.changeOwnership({}, newBelongsTo)).resolves.toBeUndefined();

      expect(containerCradle.certificateModel.model.findOneAndUpdate).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.maxTimeMS).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.exec).toHaveBeenCalledTimes(1);

      expect(containerCradle.ownershipNotifier.creation).toHaveBeenCalledTimes(1);
    });

    it('should change (remove) the ownership associated with the certificate (owner = device)', async () => {
      const oldBelongsTo = { device: '123abc' };
      const newBelongsTo = { device: null };

      // returns the found document (mock)
      containerCradle.certificateModel.model.exec = jest.fn(() => ({
        belongsTo: oldBelongsTo,
      }));

      const certificateService = new CertificateService(containerCradle);

      await expect(certificateService.changeOwnership({}, newBelongsTo)).resolves.toBeUndefined();

      expect(containerCradle.certificateModel.model.findOneAndUpdate).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.maxTimeMS).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.exec).toHaveBeenCalledTimes(1);

      expect(containerCradle.ownershipNotifier.removal).toHaveBeenCalledTimes(1);
    });

    it('should change (remove) the ownership associated with the certificate (owner = application)', async () => {
      const oldBelongsTo = { application: 'k2v' };
      const newBelongsTo = { application: null };

      // returns the found document (mock)
      containerCradle.certificateModel.model.exec = jest.fn(() => ({
        belongsTo: oldBelongsTo,
      }));

      const certificateService = new CertificateService(containerCradle);

      await expect(certificateService.changeOwnership({}, newBelongsTo)).resolves.toBeUndefined();

      expect(containerCradle.certificateModel.model.findOneAndUpdate).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.maxTimeMS).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.exec).toHaveBeenCalledTimes(1);

      expect(containerCradle.ownershipNotifier.removal).toHaveBeenCalledTimes(1);
    });

    it('should throw an exception because the certificate to be changed was not found', async () => {
      // returns no documents (mock)
      containerCradle.certificateModel.model.exec = jest.fn(() => null);

      const certificateService = new CertificateService(containerCradle);

      await expect(certificateService.changeOwnership({})).rejects.toThrow();

      expect(containerCradle.certificateModel.model.findOneAndUpdate).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.maxTimeMS).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.exec).toHaveBeenCalledTimes(1);

      expect(containerCradle.ownershipNotifier.change).toHaveBeenCalledTimes(0);
    });

    it('should throw an exception because there is more than one type of owner for the certificate', async () => {
      const certificateService = new CertificateService(containerCradle);

      await expect(certificateService.changeOwnership({}, { application: 'kafka-consumer', device: 'abc123' })).rejects.toThrow();

      expect(containerCradle.deviceMgrProvider.checkDeviceExists).toHaveBeenCalledTimes(0);

      expect(containerCradle.certificateModel.model.findOneAndUpdate).toHaveBeenCalledTimes(0);
      expect(containerCradle.certificateModel.model.maxTimeMS).toHaveBeenCalledTimes(0);
      expect(containerCradle.certificateModel.model.exec).toHaveBeenCalledTimes(0);

      expect(containerCradle.ownershipNotifier.change).toHaveBeenCalledTimes(0);
    });

    it('should throw an exception for not finding the device for the informed tenant', async () => {
      // This mock simulates that the informed device has no relation to the tenant
      containerCradle.deviceMgrProvider.checkDeviceExists = jest.fn().mockResolvedValue(false);

      const certificateService = new CertificateService(containerCradle);

      await expect(certificateService.changeOwnership({}, { device: 'abc123' })).rejects.toThrow();

      expect(containerCradle.deviceMgrProvider.checkDeviceExists).toHaveBeenCalledTimes(1);

      expect(containerCradle.certificateModel.model.findOneAndUpdate).toHaveBeenCalledTimes(0);
      expect(containerCradle.certificateModel.model.maxTimeMS).toHaveBeenCalledTimes(0);
      expect(containerCradle.certificateModel.model.exec).toHaveBeenCalledTimes(0);

      expect(containerCradle.ownershipNotifier.change).toHaveBeenCalledTimes(0);
    });
  });

  describe('retrieves a certificate from the database', () => {
    let containerCradle = null;

    beforeEach(() => {
      containerCradle = { ...containerCradleTemplate };
      containerCradle.certificateModel = certificateModelMock();
    });

    it('should obtain a certificate from the database', async () => {
      // returns the found document (mock)
      const returnMock = {};
      containerCradle.certificateModel.model.exec = jest.fn(() => (returnMock));

      const certificateService = new CertificateService(containerCradle);

      await expect(certificateService.getCertificate([], {})).resolves.toEqual(returnMock);

      expect(containerCradle.certificateModel.model.findOne).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.select).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.maxTimeMS).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.lean).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.exec).toHaveBeenCalledTimes(1);
    });

    it('should throw an exception because the certificate was not found', async () => {
      // returns no documents (mock)
      containerCradle.certificateModel.model.exec = jest.fn(() => null);

      const certificateService = new CertificateService(containerCradle);

      await expect(certificateService.getCertificate([], {})).rejects.toThrow();

      expect(containerCradle.certificateModel.model.findOne).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.select).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.maxTimeMS).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.lean).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.exec).toHaveBeenCalledTimes(1);
    });
  });

  describe('obtaining certificate list', () => {
    let containerCradle = null;

    beforeEach(() => {
      containerCradle = { ...containerCradleTemplate };
      containerCradle.certificateModel = certificateModelMock();
    });

    it('should get a list of certificates from the database', async () => {
      const itemCount = 1;
      const results = {};

      containerCradle.certificateModel.model.exec = jest.fn(() => results);
      containerCradle.certificateModel.model.countDocuments = jest.fn(() => itemCount);

      const certificateService = new CertificateService(containerCradle);

      await expect(certificateService.listCertificates([], {}))
        .resolves.toEqual({ itemCount, results });

      expect(containerCradle.certificateModel.model.find).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.select).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.limit).toHaveBeenCalledTimes(0);
      expect(containerCradle.certificateModel.model.skip).toHaveBeenCalledTimes(0);
      expect(containerCradle.certificateModel.model.maxTimeMS).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.lean).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.exec).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.countDocuments).toHaveBeenCalledTimes(1);
    });
  });

  describe('obtaining certificate list (with limit and offset)', () => {
    let containerCradle = null;

    beforeEach(() => {
      containerCradle = { ...containerCradleTemplate };
      containerCradle.certificateModel = certificateModelMock();
    });

    it('should get a list of certificates from the database', async () => {
      const itemCount = 1;
      const results = {};

      containerCradle.certificateModel.model.exec = jest.fn(() => results);
      containerCradle.certificateModel.model.countDocuments = jest.fn(() => itemCount);

      const certificateService = new CertificateService(containerCradle);

      await expect(certificateService.listCertificates([], {}, 1, 1))
        .resolves.toEqual({ itemCount, results });

      expect(containerCradle.certificateModel.model.find).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.select).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.limit).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.skip).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.maxTimeMS).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.lean).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.exec).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.countDocuments).toHaveBeenCalledTimes(1);
    });
  });

  describe('certificate removal', () => {
    let containerCradle = null;

    beforeEach(() => {
      containerCradle = { ...containerCradleTemplate };
      containerCradle.certificateModel = certificateModelMock();
      containerCradle.pkiUtils = pkiUtilsMock();
    });

    it('should remove the certificate from the database (issued by dojot)', async () => {
      containerCradle.pkiUtils.parseCert = jest.fn(() => ({ issuer: {} }));

      const certificateService = new CertificateService(containerCradle);

      await expect(certificateService.deleteCertificate({
        _id: 1,
        pem: util.p256Cert,
        issuedByDojotPki: true, // issued by dojot
      })).resolves.toBeUndefined();

      expect(containerCradle.certificateModel.model.findByIdAndDelete).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.maxTimeMS).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.exec).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.parseCert).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.getSerialNumber).toHaveBeenCalledTimes(1);
      expect(containerCradle.dnUtils.from).toHaveBeenCalledTimes(1);
      expect(containerCradle.dnUtils.from.dn.stringify).toHaveBeenCalledTimes(1);
      expect(containerCradle.ejbcaFacade.revokeCertificate).toHaveBeenCalledTimes(1);

      expect(containerCradle.ownershipNotifier.removal).toHaveBeenCalledTimes(1);
    });

    it('should remove the certificate from the database (issued by external CA)', async () => {
      const certificateService = new CertificateService(containerCradle);

      await expect(certificateService.deleteCertificate({
        _id: 1,
        pem: util.p256Cert,
        issuedByDojotPki: false, // issued by external CA
      })).resolves.toBeUndefined();

      expect(containerCradle.certificateModel.model.findByIdAndDelete).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.maxTimeMS).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.exec).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.parseCert).toHaveBeenCalledTimes(0);
      expect(containerCradle.pkiUtils.getSerialNumber).toHaveBeenCalledTimes(0);
      expect(containerCradle.dnUtils.from).toHaveBeenCalledTimes(0);
      expect(containerCradle.dnUtils.from.dn.stringify).toHaveBeenCalledTimes(0);
      expect(containerCradle.ejbcaFacade.revokeCertificate).toHaveBeenCalledTimes(0);

      expect(containerCradle.ownershipNotifier.removal).toHaveBeenCalledTimes(1);
    });
  });

  describe('certificate issuance without registration control', () => {
    let containerCradle = null;

    beforeEach(() => {
      containerCradle = { ...containerCradleTemplate };

      containerCradle.ejbcaFacade = ejbcaFacadeMock();
      containerCradle.ejbcaFacade.generateCertificate = jest.fn(() => util.p256Cert);

      containerCradle.pkiUtils = pkiUtilsMock();
      containerCradle.pkiUtils.getFingerprint = jest.fn(() => util.p256CertFingerprint);
    });

    it('should throw away a certificate', async () => {
      containerCradle.pkiUtils.parseCSR = jest.fn(() => ({ subject: {} }));

      const certificateService = new CertificateService(containerCradle);

      await expect(certificateService.throwAwayCertificate({
        csr: util.p256CSR,
      })).resolves.toEqual({
        certificateFingerprint: util.p256CertFingerprint,
        certificatePem: util.p256Cert,
      });

      expect(containerCradle.pkiUtils.parseCSR).toHaveBeenCalledTimes(1);
      expect(containerCradle.dnUtils.from).toHaveBeenCalledTimes(1);
      expect(containerCradle.dnUtils.from.dn.stringify).toHaveBeenCalledTimes(1);
      expect(containerCradle.ejbcaFacade.generateCertificate).toHaveBeenCalledTimes(1);

      expect(containerCradle.ownershipNotifier.creation).toHaveBeenCalledTimes(1);
    });
  });
});
