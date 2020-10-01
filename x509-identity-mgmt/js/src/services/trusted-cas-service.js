const { BadRequest, NotFound, Conflict } = require('../sdk/web/backing/error-template');

const pkiUtils = require('../core/pki-utils');

/**
 * Service to handle Trusted CAs
 */
class TrustedCAsService {
  /**
   * The dependencies are injected through the constructor
   */
  constructor({
    db, ejbcaFacade, tenant, rootCA, externalCaCertMinimumValidityDays, queryMaxTimeMS, caCertLimit,
  }) {
    this.db = db;
    this.ejbcaFacade = ejbcaFacade;
    this.tenant = tenant;
    this.rootCA = rootCA;
    this.queryMaxTimeMS = queryMaxTimeMS;
    this.externalCaCertMinimumValidityDays = externalCaCertMinimumValidityDays;
    this.TrustedCAModel = db.trustedCA.model;
    this.caCertLimit = caCertLimit;
  }

  /**
   * Obtains the Root certificate used to sign all certificates generated by this component.
   *
   * @returns the root CA certificate.
   */
  async getRootCertificate() {
    const caPem = await this.ejbcaFacade.getRootCertificate(this.rootCA);

    const certificateFingerprint = pkiUtils.getFingerprint(caPem);

    return { certificateFingerprint, caPem };
  }

  /**
   * Get the latest valid certificate revocation list.
   *
   * @param {boolean} renew Flag indicating whether a new CRL should
   *                        be generated in the EJBCA request
   *
   * @returns the latest valid CRL.
   */
  async getRootCRL(renew = false) {
    const crl = await this.ejbcaFacade.getCRL(this.rootCA, renew);
    return { crl };
  }

  /**
   * Retrieves a trusted CA certificate from the database.
   *
   * @param {object} queryFields Certificate fields that must be returned in the record
   * @param {object} filterFields Filter fields to find the correct certificate in the database
   *
   * @returns Returns the record that represents the certificate in the database
   *
   * @throws an exception if no record is found with the informed filters.
   */
  async getTrustedCACertificate(queryFields, filterFields) {
    Object.assign(filterFields, { tenant: this.tenant });

    /* Executes the query and converts the result to JSON */
    const result = await this.TrustedCAModel.findOne(filterFields)
      .maxTimeMS(this.queryMaxTimeMS)
      .lean()
      .exec();
    if (!result) {
      throw NotFound(`No records found for the following parameters: ${JSON.stringify(filterFields)}`);
    }
    return result;
  }

  /**
   * Retrieves from the database a set of trusted CA certificates that meet the search criteria.
   *
   * @param {object} queryFields Certificate fields that must be returned in each record.
   * @param {object} filterFields Filter fields to find the correct certificates in the database.
   * @param {number} limit Limit of records that must be returned.
   * @param {number} offset Offset in relation to the first record found by the query.
   *
   * @returns a set of certificates that meet the search criteria.
   */
  async listTrustedCACertificates(queryFields, filterFields, limit, offset) {
    Object.assign(filterFields, { tenant: this.tenant });

    /* Executes the query and converts the results to JSON */
    const [results, itemCount] = await Promise.all([
      this.TrustedCAModel.find(filterFields)
        .select(queryFields.join(' '))
        .limit(limit).skip(offset)
        .maxTimeMS(this.queryMaxTimeMS)
        .lean()
        .exec(),
      this.TrustedCAModel.countDocuments(filterFields),
    ]);
    return { itemCount, results };
  }

  /**
   * Register an external trusted root CA certificate (not generated by this service).
   *
   * @param {object} Object with the root CA certificate.
   *
   * @returns the fingerprint of the registered certificate.
   */
  async registerTrustedCACertificate({ caPem, allowAutoRegistration }) {
    const caCert = pkiUtils.parseCert(caPem);

    pkiUtils.checkRemainingDays(caCert, this.externalCaCertMinimumValidityDays);

    await pkiUtils.assertRootCA(caCert);

    pkiUtils.checkRootExternalCN(caCert, this.rootCA);

    await this.checkCertLimit();

    const caFingerprint = pkiUtils.getFingerprint(caPem);

    await this.checkExistingCertificate(caFingerprint);

    const model = new this.TrustedCAModel({
      caFingerprint,
      caPem,
      allowAutoRegistration,
      tenant: this.tenant,
    });
    await model.save();

    return { caFingerprint };
  }

  /**
   * Removes a external trusted root CA certificate from the database.
   *
   * @param {object} certRecord Record that represents the certificate in
   *                       the database and that must be removed.
   */
  async deleteCertificate(certRecord) {
    /* eslint no-underscore-dangle: ["error", { "allow": ["_id"] }] */
    await this.TrustedCAModel.findByIdAndDelete(certRecord._id)
      .maxTimeMS(this.queryMaxTimeMS)
      .exec();
  }

  /**
   * checks the CA certificates limit that can be registered by tenant.
   *
   * @throws an exception if the maximum number has already been reached and
   * a new certificate is trying to be inserted.
   */
  async checkCertLimit() {
    if (this.caCertLimit > -1) {
      const filterFields = { tenant: this.tenant };
      const count = await this.TrustedCAModel.countDocuments(filterFields);
      if (count >= this.caCertLimit) {
        throw BadRequest('The number of registered CAs has been exceeded!');
      }
    }
  }

  /**
   * Checks if there is already a certificate registered in the database with
   * the same fingerprint informed by parameter.
   *
   * @param {string} fingerprint fingerprint to be used as a query filter.
   *
   * @throws an exception if there is already a certificate registered in the
   * database with the same fingerprint informed by parameter.
   */
  async checkExistingCertificate(fingerprint) {
    const filterFields = {
      caFingerprint: fingerprint,
      tenant: this.tenant,
    };
    const count = await this.TrustedCAModel.countDocuments(filterFields);
    if (count) {
      throw Conflict(`The certificate with fingerprint '${fingerprint}' already exists!`);
    }
  }
}

module.exports = TrustedCAsService;
