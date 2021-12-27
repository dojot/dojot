const { WebUtils } = require('@dojot/microservice-sdk');

const TrustedCAService = require('../../../src/services/TrustedCAService');

const util = require('../../util.test');

let containerCradleTemplate = null;

function modelMock() {
  const modelInstance = {
    save: jest.fn(),
  };

  const mock = {
    model: jest.fn().mockImplementation(() => modelInstance),
    parseConditionFields: jest.fn(),
  };

  const modelRef = mock.model;
  modelRef.instance = modelInstance;
  modelRef.find = jest.fn(() => modelRef);
  modelRef.findOne = jest.fn(() => modelRef);
  modelRef.findOneAndUpdate = jest.fn(() => modelRef);
  modelRef.findByIdAndDelete = jest.fn(() => modelRef);
  modelRef.deleteMany = jest.fn(() => modelRef);
  modelRef.select = jest.fn(() => modelRef);
  modelRef.limit = jest.fn(() => modelRef);
  modelRef.skip = jest.fn(() => modelRef);
  modelRef.maxTimeMS = jest.fn(() => modelRef);
  modelRef.lean = jest.fn(() => modelRef);
  modelRef.aggregate = jest.fn(() => modelRef);
  modelRef.group = jest.fn(() => modelRef);
  modelRef.exec = jest.fn();
  modelRef.countDocuments = jest.fn();

  return mock;
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
    assertRootCA: jest.fn(),
    checkRootExternalCN: jest.fn(),
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

function trustedCANotifierMock() {
  const trustedCANotifier = {
    creation: jest.fn().mockResolvedValue(undefined),
    removal: jest.fn().mockResolvedValue(undefined),
  };
  return trustedCANotifier;
}

beforeAll(() => {
  const certificateModel = modelMock();

  const trustedCAModel = modelMock();

  const trustedCANotifier = trustedCANotifierMock();

  const pkiUtils = pkiUtilsMock();

  const dnUtils = dnUtilsMock();

  containerCradleTemplate = {
    certificateModel,
    trustedCAModel,
    trustedCANotifier,
    pkiUtils,
    dnUtils,
    tenant: 'admin',
    rootCA: global.config.ejbca.rootca,
    caCertLimit: global.config.certificate.external.ca.limit,
    queryMaxTimeMS: global.config.mongo.query.maxtimems,
    externalCaCertMinimumValidityDays: global.config.certificate.external.ca.minimumvaliditydays,
    errorTemplate: WebUtils.framework.errorTemplate,
  };
});

describe("Unit tests of script 'TrustedCAService.js'", () => {
  afterEach(() => {
    jest.clearAllMocks();
  });

  describe('registration of trusted CA certificates', () => {
    let containerCradle = null;

    beforeEach(() => {
      containerCradle = { ...containerCradleTemplate };
      containerCradle.trustedCAModel = modelMock();

      containerCradle.pkiUtils = pkiUtilsMock();
      containerCradle.pkiUtils.getFingerprint = jest.fn(() => util.caFingerprint);
    });

    it('should register a trusted CA certificate', async () => {
      const trustedCAService = new TrustedCAService(containerCradle);

      await expect(trustedCAService.registerCertificate({
        caPem: util.caCert,
        allowAutoRegistration: false,
      })).resolves.toEqual({
        caFingerprint: util.caFingerprint,
      });

      expect(containerCradle.pkiUtils.parseCert).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.getFingerprint).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.checkRemainingDays).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.assertRootCA).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.checkRootExternalCN).toHaveBeenCalledTimes(1);

      expect(containerCradle.dnUtils.from).toHaveBeenCalledTimes(1);
      expect(containerCradle.dnUtils.from.dn.stringify).toHaveBeenCalledTimes(1);

      expect(containerCradle.trustedCAModel.model).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.instance.save).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.countDocuments).toHaveBeenCalledTimes(1);

      expect(containerCradle.trustedCANotifier.creation).toHaveBeenCalledTimes(1);
    });

    it('should register a trusted CA certificate (checking the limit by tenant)', async () => {
      containerCradle.caCertLimit = 1; // checking the limit by tenant

      const trustedCAService = new TrustedCAService(containerCradle);

      await expect(trustedCAService.registerCertificate({
        caPem: util.caCert,
        allowAutoRegistration: false,
      })).resolves.toEqual({
        caFingerprint: util.caFingerprint,
      });

      expect(containerCradle.pkiUtils.parseCert).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.getFingerprint).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.checkRemainingDays).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.assertRootCA).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.checkRootExternalCN).toHaveBeenCalledTimes(1);

      expect(containerCradle.dnUtils.from).toHaveBeenCalledTimes(1);
      expect(containerCradle.dnUtils.from.dn.stringify).toHaveBeenCalledTimes(1);

      expect(containerCradle.trustedCAModel.model).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.instance.save).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.countDocuments).toHaveBeenCalledTimes(2);

      expect(containerCradle.trustedCANotifier.creation).toHaveBeenCalledTimes(1);
    });

    it('should throw an exception because the number of registered CAs has been exceeded', async () => {
      containerCradle.caCertLimit = 1; // checking the limit by tenant

      containerCradle.trustedCAModel.model.countDocuments = jest.fn(() => 1);

      const trustedCAService = new TrustedCAService(containerCradle);

      await expect(trustedCAService.registerCertificate({
        caPem: util.caCert,
        allowAutoRegistration: false,
      })).rejects.toThrow();

      expect(containerCradle.pkiUtils.parseCert).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.getFingerprint).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.checkRemainingDays).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.assertRootCA).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.checkRootExternalCN).toHaveBeenCalledTimes(1);

      expect(containerCradle.dnUtils.from).toHaveBeenCalledTimes(0);
      expect(containerCradle.dnUtils.from.dn.stringify).toHaveBeenCalledTimes(0);

      expect(containerCradle.trustedCAModel.model).toHaveBeenCalledTimes(0);
      expect(containerCradle.trustedCAModel.model.instance.save).toHaveBeenCalledTimes(0);
      expect(containerCradle.trustedCAModel.model.countDocuments).toHaveBeenCalledTimes(1);

      expect(containerCradle.trustedCANotifier.creation).toHaveBeenCalledTimes(0);
    });

    it('should throw an exception because the certificate already exists', async () => {
      containerCradle.caCertLimit = 1; // checking the limit by tenant

      containerCradle.trustedCAModel.model.countDocuments = jest.fn()
        .mockImplementationOnce(() => 0)
        .mockImplementationOnce(() => 1);

      const trustedCAService = new TrustedCAService(containerCradle);

      await expect(trustedCAService.registerCertificate({
        caPem: util.caCert,
        allowAutoRegistration: false,
      })).rejects.toThrow();

      expect(containerCradle.pkiUtils.parseCert).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.getFingerprint).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.checkRemainingDays).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.assertRootCA).toHaveBeenCalledTimes(1);
      expect(containerCradle.pkiUtils.checkRootExternalCN).toHaveBeenCalledTimes(1);

      expect(containerCradle.dnUtils.from).toHaveBeenCalledTimes(0);
      expect(containerCradle.dnUtils.from.dn.stringify).toHaveBeenCalledTimes(0);

      expect(containerCradle.trustedCAModel.model).toHaveBeenCalledTimes(0);
      expect(containerCradle.trustedCAModel.model.instance.save).toHaveBeenCalledTimes(0);
      expect(containerCradle.trustedCAModel.model.countDocuments).toHaveBeenCalledTimes(2);

      expect(containerCradle.trustedCANotifier.creation).toHaveBeenCalledTimes(0);
    });
  });


  describe('obtaining CA certificates', () => {
    let containerCradle = null;

    beforeEach(() => {
      containerCradle = { ...containerCradleTemplate };
      containerCradle.trustedCAModel = modelMock();
    });

    it('should obtain a trusted CA certificate', async () => {
      // returns the found document (mock)
      const returnMock = {};
      containerCradle.trustedCAModel.model.exec = jest.fn(() => (returnMock));

      const trustedCAService = new TrustedCAService(containerCradle);

      await expect(trustedCAService.getCertificate([], {})).resolves.toEqual(returnMock);

      expect(containerCradle.trustedCAModel.model.findOne).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.select).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.maxTimeMS).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.lean).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.exec).toHaveBeenCalledTimes(1);
    });

    it('should obtain a trusted CA certificate bundle', async () => {
      // returns the found document (mock)
      const returnAggregation = [{
        _id: util.caFingerprint,
        caPem: util.caCert,
      }];
      containerCradle.trustedCAModel.model.exec = jest.fn(() => (returnAggregation));

      const trustedCAService = new TrustedCAService(containerCradle);

      await expect(trustedCAService.getCertificateBundle())
        .resolves.toEqual([util.caCert]);

      expect(containerCradle.trustedCAModel.model.aggregate).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.group).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.exec).toHaveBeenCalledTimes(1);
    });

    it('should throw an exception because the trusted CA certificate was not found', async () => {
      // returns no documents (mock)
      containerCradle.trustedCAModel.model.exec = jest.fn(() => null);

      const trustedCAService = new TrustedCAService(containerCradle);

      await expect(trustedCAService.getCertificate([], {})).rejects.toThrow();

      expect(containerCradle.trustedCAModel.model.findOne).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.select).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.maxTimeMS).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.lean).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.exec).toHaveBeenCalledTimes(1);
    });

    it('should get a list of trusted CA certificates from the database', async () => {
      const itemCount = 1;
      const results = {};

      containerCradle.trustedCAModel.model.exec = jest.fn(() => results);
      containerCradle.trustedCAModel.model.countDocuments = jest.fn(() => itemCount);

      const trustedCAService = new TrustedCAService(containerCradle);

      await expect(trustedCAService.listCertificates(
        [], {}, 1, 0,
      ))
        .resolves.toEqual({ itemCount, results });

      expect(containerCradle.trustedCAModel.model.find).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.select).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.limit).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.skip).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.maxTimeMS).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.lean).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.exec).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.countDocuments).toHaveBeenCalledTimes(1);
    });
  });

  describe('self registration', () => {
    let containerCradle = null;

    beforeEach(() => {
      containerCradle = { ...containerCradleTemplate };
      containerCradle.trustedCAModel = modelMock();
    });

    it('should change the self-registration of certificates issued by the trusted CA', async () => {
      const results = {};
      containerCradle.trustedCAModel.model.exec = jest.fn(() => results);

      const trustedCAService = new TrustedCAService(containerCradle);

      await expect(trustedCAService.changeAutoRegistration({}, true))
        .resolves.toBeUndefined();

      expect(containerCradle.trustedCAModel.model.findOneAndUpdate).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.maxTimeMS).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.exec).toHaveBeenCalledTimes(1);
    });

    it('should throw an exception because the trusted CA certificate was not found', async () => {
      const results = null;
      containerCradle.trustedCAModel.model.exec = jest.fn(() => results);

      const trustedCAService = new TrustedCAService(containerCradle);

      await expect(trustedCAService.changeAutoRegistration({}, true))
        .rejects.toThrow();

      expect(containerCradle.trustedCAModel.model.findOneAndUpdate).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.maxTimeMS).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.exec).toHaveBeenCalledTimes(1);
    });
  });

  describe('removing trusted CA certificate', () => {
    let containerCradle = null;

    beforeEach(() => {
      containerCradle = { ...containerCradleTemplate };
      containerCradle.trustedCAModel = modelMock();
      containerCradle.certificateModel = modelMock();
    });

    it('should remove the certificate from the CA and its self-registered', async () => {
      const trustedCAService = new TrustedCAService(containerCradle);

      await expect(trustedCAService.deleteCertificate({ caFingerprint: util.caFingerprint }))
        .resolves.toBeUndefined();

      expect(containerCradle.certificateModel.parseConditionFields).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.countDocuments).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.deleteMany).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.maxTimeMS).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.exec).toHaveBeenCalledTimes(1);

      expect(containerCradle.trustedCAModel.model.findByIdAndDelete).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.maxTimeMS).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.exec).toHaveBeenCalledTimes(1);

      expect(containerCradle.trustedCANotifier.removal).toHaveBeenCalledTimes(1);
    });

    it('should throw an exception because there are certificates that have not been self-registered', async () => {
      containerCradle.certificateModel.model.countDocuments = jest.fn(() => 1);

      const trustedCAService = new TrustedCAService(containerCradle);

      await expect(trustedCAService.deleteCertificate({ caFingerprint: util.caFingerprint }))
        .rejects.toThrow();

      expect(containerCradle.certificateModel.parseConditionFields).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.countDocuments).toHaveBeenCalledTimes(1);
      expect(containerCradle.certificateModel.model.deleteMany).toHaveBeenCalledTimes(0);
      expect(containerCradle.certificateModel.model.maxTimeMS).toHaveBeenCalledTimes(0);
      expect(containerCradle.certificateModel.model.exec).toHaveBeenCalledTimes(0);

      expect(containerCradle.trustedCAModel.model.findByIdAndDelete).toHaveBeenCalledTimes(0);
      expect(containerCradle.trustedCAModel.model.maxTimeMS).toHaveBeenCalledTimes(0);
      expect(containerCradle.trustedCAModel.model.exec).toHaveBeenCalledTimes(0);

      expect(containerCradle.trustedCANotifier.removal).toHaveBeenCalledTimes(0);
    });
  });

  describe('obtaining CA certificate in PEM format', () => {
    let containerCradle = null;

    beforeEach(() => {
      containerCradle = { ...containerCradleTemplate };
      containerCradle.trustedCAModel = modelMock();
      containerCradle.certificateModel = modelMock();
    });

    it('should obtain the PEM from the trusted CA certificate', async () => {
      containerCradle.trustedCAModel.model.exec = jest.fn(() => ({ caPem: util.caCert }));

      const trustedCAService = new TrustedCAService(containerCradle);

      await expect(trustedCAService.getPEM(util.caFingerprint))
        .resolves.toEqual(util.caCert);

      expect(containerCradle.trustedCAModel.parseConditionFields).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.findOne).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.select).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.maxTimeMS).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.lean).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.exec).toHaveBeenCalledTimes(1);
    });

    it('should throw an exception because no CA certificate was found', async () => {
      containerCradle.trustedCAModel.model.exec = jest.fn(() => null);

      const trustedCAService = new TrustedCAService(containerCradle);

      await expect(trustedCAService.getPEM(util.caFingerprint))
        .rejects.toThrow();

      expect(containerCradle.trustedCAModel.parseConditionFields).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.findOne).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.select).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.maxTimeMS).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.lean).toHaveBeenCalledTimes(1);
      expect(containerCradle.trustedCAModel.model.exec).toHaveBeenCalledTimes(1);
    });
  });
});
