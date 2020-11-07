/**
 * Service to handle certificates
 */
class CertificateService {
  /**
   * The dependencies are injected through the constructor
   */
  constructor({
    trustedCAService, certificateModel, ejbcaFacade, tenant, pkiUtils,
    dnUtils, certValidity, checkPublicKey, queryMaxTimeMS, certMinimumValidityDays,
    caCertAutoRegistration, logger, errorTemplate,
  }) {
    Object.defineProperty(this, 'trustedCAService', { value: trustedCAService });
    Object.defineProperty(this, 'ejbcaFacade', { value: ejbcaFacade });
    Object.defineProperty(this, 'tenant', { value: tenant });
    Object.defineProperty(this, 'pkiUtils', { value: pkiUtils });
    Object.defineProperty(this, 'dnUtils', { value: dnUtils });
    Object.defineProperty(this, 'certValidity', { value: certValidity });
    Object.defineProperty(this, 'checkPublicKey', { value: checkPublicKey });
    Object.defineProperty(this, 'queryMaxTimeMS', { value: queryMaxTimeMS });
    Object.defineProperty(this, 'certMinimumValidityDays', { value: certMinimumValidityDays });
    Object.defineProperty(this, 'caCertAutoRegistration', { value: caCertAutoRegistration });
    Object.defineProperty(this, 'logger', { value: logger });
    Object.defineProperty(this, 'CertificateModel', { value: certificateModel.model });
    Object.defineProperty(this, 'error', { value: errorTemplate });
  }

  /**
   * Generates an x509 certificate based on a CSR
   *
   * @param {object} Object with the CSR and data whom the certificate is to be associated.
   *
   * @returns an object containing the certificate in PEM format and its fingerprint.
   */
  async generateCertificate({ csr: csrPem, belongsTo }) {
    const csr = this.pkiUtils.parseCSR(csrPem);

    if (this.checkPublicKey) {
      this.pkiUtils.checkPublicKey(csr.subjectPublicKeyInfo);
    }

    const subjectDN = this.dnUtils.from(csr.subject, true)
      .verify()
      .cnamePrefix(this.tenant)
      .stringify();

    const certificatePem = await this.ejbcaFacade.generateCertificate(
      subjectDN, this.certValidity, csrPem,
    );

    const cert = this.pkiUtils.parseCert(certificatePem);
    const certificateFingerprint = this.pkiUtils.getFingerprint(certificatePem);

    const model = new this.CertificateModel({
      fingerprint: certificateFingerprint,
      pem: certificatePem,
      belongsTo,
      subjectDN,
      validity: {
        notBefore: cert.notBefore.value,
        notAfter: cert.notAfter.value,
      },
      tenant: this.tenant,
    });
    await model.save();

    return { certificateFingerprint, certificatePem };
  }

  /**
    * Register an external certificate (not generated by this service)
    *
    * @param {object} Object with the certificate to be registered.
    *
    * @returns the fingerprint of the registered certificate.
    */
  async registerCertificate({ caFingerprint, certificateChain, belongsTo }) {
    let rootCAPem = null;
    let rootCAFingerprint = caFingerprint;

    // the first certificate in the chain must be the one to be registered
    const [pemToBeRegistered] = certificateChain;
    const certToBeRegistered = this.pkiUtils.parseCert(pemToBeRegistered);
    const certFingerprint = this.pkiUtils.getFingerprint(pemToBeRegistered);

    // ensures that the certificate's validity is within the limit
    this.pkiUtils.checkRemainingDays(certToBeRegistered, this.certMinimumValidityDays);

    // ensures that the certificate is not a CA certificate
    // and that it is also not self-signed
    await this.pkiUtils.assertLeaf(certToBeRegistered);

    // ensures that the certificate has not yet been registered for the tentant
    await this.checkExistingCertificate(certFingerprint);

    // Attempts to extract the root CA from the certificate chain
    if (certificateChain.length > 1) {
      const lastCert = this.pkiUtils.parseCert(certificateChain[certificateChain.length - 1]);
      if (await this.pkiUtils.isRootCA(lastCert)) {
        rootCAPem = certificateChain.pop();
        rootCAFingerprint = this.pkiUtils.getFingerprint(rootCAPem);

        // If "caFingerprint" is informed and also a root CA certificate in the payload,
        // we must ensure that the two are the same, otherwise, we must return an error.
        if (caFingerprint && caFingerprint !== rootCAFingerprint) {
          throw this.error.BadRequest('The fingerprint of the CA defined in the payload of the request '
            + 'does not match the fingerprint of the certificate of the root CA that was '
            + 'also informed in the certificate chain.');
        }
      }
    }

    if (!rootCAFingerprint) {
      throw this.error.BadRequest('It was not possible to obtain the fingerprint of the root CA. '
        + 'Make sure to inform it in the request payload or inform the root CA '
        + 'certificate as the last one in the chain of certificates.');
    }

    // Attempts to obtain the previously registered root CA certificate...
    let registeredRootCAPem = null;
    try {
      registeredRootCAPem = await this.trustedCAService.getPEM(rootCAFingerprint);
    } catch (ex) {
      this.logger.debug(ex);
      // If the root CA certificate has not been registered, but it was informed in
      // the request and the service is able to automatically register it, then it does.
      // Otherwise, it throws an error and does not continue with the operation.
      if (this.caCertAutoRegistration && rootCAPem) {
        await this.trustedCAService.registerCertificate({
          caPem: rootCAPem, allowAutoRegistration: false,
        });
        registeredRootCAPem = rootCAPem;
      } else {
        throw this.error.BadRequest('The root CA certificate is not registered as a trusted certificate yet.');
      }
    }

    // The last certificate of the chain must be that of the root CA previously registered
    certificateChain.push(registeredRootCAPem);

    // Converts the PEM format certificate chain to javascript objects
    const certChain = certificateChain.map((pem) => this.pkiUtils.parseCert(pem));

    // ensures that the certificate chain of trust is valid
    const trusted = certChain[certChain.length - 1];
    await this.pkiUtils.checkChainOfTrust(certChain, trusted);

    // Register the certificate in the database
    const subjectDN = this.dnUtils.from(certToBeRegistered.subject).stringify();
    const model = new this.CertificateModel({
      caFingerprint: rootCAFingerprint,
      fingerprint: certFingerprint,
      pem: pemToBeRegistered,
      subjectDN,
      validity: {
        notBefore: certToBeRegistered.notBefore.value,
        notAfter: certToBeRegistered.notAfter.value,
      },
      issuedByDojotPki: false,
      belongsTo,
      tenant: this.tenant,
    });
    await model.save();

    return { certificateFingerprint: certFingerprint };
  }

  /**
   * Changes the one that is associated with a certificate
   *
   * @param {object} filterFields Filter fields to find the correct certificate in the database
   * @param {object} belongsTo Data of whom the certificate should be associated
   *
   * @throws an exception if no record is found with the entered filters.
   */
  async changeOwnership(filterFields, belongsTo) {
    Object.assign(filterFields, { tenant: this.tenant });

    const result = await this.CertificateModel.findOneAndUpdate(
      filterFields, { belongsTo, modifiedAt: new Date() },
    ).maxTimeMS(this.queryMaxTimeMS).exec();

    if (!result) {
      throw this.error.NotFound(`No records found for the following parameters: ${JSON.stringify(filterFields)}`);
    }
  }

  /**
   * Retrieves a certificate from the database
   *
   * @param {object} queryFields Certificate fields that must be returned in the record
   * @param {object} filterFields Filter fields to find the correct certificate in the database
   *
   * @returns Returns the record that represents the certificate in the database
   *
   * @throws an exception if no record is found with the informed filters.
   */
  async getCertificate(queryFields, filterFields) {
    Object.assign(filterFields, { tenant: this.tenant });

    /* Executes the query and converts the result to JSON */
    const result = await this.CertificateModel.findOne(filterFields)
      .select(queryFields.join(' '))
      .maxTimeMS(this.queryMaxTimeMS)
      .lean()
      .exec();

    if (!result) {
      throw this.error.NotFound(`No records found for the following parameters: ${JSON.stringify(filterFields)}`);
    }

    return result;
  }

  /**
   * Retrieves from the database a set of certificates that meet the search criteria
   *
   * @param {object} queryFields Certificate fields that must be returned in each record
   * @param {object} filterFields Filter fields to find the correct certificates in the database
   * @param {number} limit Limit of records that must be returned
   * @param {number} offset Offset in relation to the first record found by the query
   *
   * @returns a set of certificates that meet the search criteria
   */
  async listCertificates(queryFields, filterFields, limit, offset) {
    Object.assign(filterFields, { tenant: this.tenant });

    /* Executes the query and converts the results to JSON */
    const [results, itemCount] = await Promise.all([
      this.CertificateModel.find(filterFields)
        .select(queryFields.join(' '))
        .limit(limit).skip(offset)
        .maxTimeMS(this.queryMaxTimeMS)
        .lean()
        .exec(),
      this.CertificateModel.countDocuments(filterFields),
    ]);

    return { itemCount, results };
  }

  /**
   * Removes a certificate from the database.
   * If the certificate was generated by this service,
   * then it will also be revoked in a CRL.
   *
   * @param {object} certRecord Record that represents the certificate in
   *                       the database and that must be removed.
   */
  async deleteCertificate(certRecord) {
    /* eslint no-underscore-dangle: ["error", { "allow": ["_id"] }] */
    await this.CertificateModel.findByIdAndDelete(certRecord._id)
      .maxTimeMS(this.queryMaxTimeMS)
      .exec();

    /* If the certificate was issued by the internal CA,
     * it must be revoked in a Certificate Revocation List */
    if (certRecord.issuedByDojotPki) {
      const cert = this.pkiUtils.parseCert(certRecord.pem);

      const issuerDN = this.dnUtils.from(cert.issuer).stringify();

      const certificateSN = this.pkiUtils.getSerialNumber(cert);

      await this.ejbcaFacade.revokeCertificate(issuerDN, certificateSN);
    }
  }

  /**
   * It literally throws away a certificate, that is, it generates a certificate,
   * but does not keep a record for it.
   * This is used by internal services that need a certificate to establish a
   * mutual TLS with other parties.
   * @param {object} Object with a CSR that will serve as the basis for generating the certificate.
   *
   * @returns an object containing the certificate in PEM format and its fingerprint.
   */
  async throwAwayCertificate({ csr: csrPem }) {
    const csr = this.pkiUtils.parseCSR(csrPem);

    const subjectDN = this.dnUtils.from(csr.subject).stringify();

    const certificatePem = await this.ejbcaFacade.generateCertificate(
      subjectDN, this.certValidity, csrPem,
    );

    const certificateFingerprint = this.pkiUtils.getFingerprint(certificatePem);

    return { certificateFingerprint, certificatePem };
  }

  /**
   * Checks if there is already a certificate registered in the database with
   * the same fingerprint informed by parameter.
   *
   * @param {string} fingerprint to be used as a query filter.
   *
   * @throws an exception if there is already a certificate registered in the
   * database with the same fingerprint informed by parameter.
   */
  async checkExistingCertificate(fingerprint) {
    const filterFields = { fingerprint, tenant: this.tenant };
    const count = await this.CertificateModel.countDocuments(filterFields);
    if (count) {
      throw this.error.Conflict(`The certificate with fingerprint '${fingerprint}' already exists.`);
    }
  }
}

module.exports = CertificateService;
