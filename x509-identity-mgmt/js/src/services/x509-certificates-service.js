const ejbcaFacade = require('../core/ejbca-facade');

const {
  parseCSR, parseCert, checkPublicKey, getFingerprint, getSerialNumber,
} = require('../core/pki-utils');

const DN = require('../core/distinguished-name');

const { NotFound } = require('../core/errors');

const { certificate: { model: CertificateModel } } = require('../db');

const { certificate: certCfg, mongo: { query: queryCfg } } = require('../config');

async function generateCertificate({ csr: csrPem, belongsTo }, tenant) {
  const csr = parseCSR(csrPem);

  checkPublicKey(csr.subjectPublicKeyInfo);

  const subjectDN = DN.from(csr.subject).verify().stringify();

  const certificatePem = await ejbcaFacade.generateCertificate(
    subjectDN, certCfg.validity, csrPem,
  );

  const certificateFingerprint = getFingerprint(certificatePem);

  const model = new CertificateModel({
    fingerprint: certificateFingerprint,
    pem: certificatePem,
    belongsTo,
    tenant,
  });
  await model.save();

  return { certificateFingerprint, certificatePem };
}

async function registerCertificate(/* { certificatePem, belongsTo }, tenant */) {
  await Promise.reject(NotFound('This operation is not available yet'));
}

async function changeOwnership(filterFields, belongsTo) {
  const result = await CertificateModel.findOneAndUpdate(filterFields, { belongsTo })
    .maxTimeMS(queryCfg.maxTimeMS)
    .exec();
  if (!result) {
    throw NotFound(`No records found for the following parameters: ${JSON.stringify(filterFields)}`);
  }
}

async function getCertificate(queryFields, filterFields) {
  /* Executes the query and converts the result to JSON */
  const result = await CertificateModel.findOne(filterFields)
    .select(queryFields.join(' '))
    .maxTimeMS(queryCfg.maxTimeMS)
    .lean()
    .exec();
  if (!result) {
    throw NotFound(`No records found for the following parameters: ${JSON.stringify(filterFields)}`);
  }
  return result;
}

async function listCertificates(queryFields, filterFields, limit, offset) {
  /* Executes the query and converts the results to JSON */
  const [results, itemCount] = await Promise.all([
    CertificateModel.find(filterFields)
      .select(queryFields.join(' '))
      .limit(limit).skip(offset)
      .maxTimeMS(queryCfg.maxTimeMS)
      .lean()
      .exec(),
    CertificateModel.count(filterFields),
  ]);
  return { itemCount, results };
}

async function deleteCertificate(certRecord) {
  /* eslint no-underscore-dangle: ["error", { "allow": ["_id"] }] */
  await CertificateModel.findByIdAndDelete(certRecord._id)
    .maxTimeMS(queryCfg.maxTimeMS)
    .exec();

  /* If the certificate was issued by the internal CA,
   * it must be revoked in a Certificate Revocation List */
  if (certRecord.issuedByDojotPki) {
    const cert = parseCert(certRecord.pem);
    const issuerDN = DN.from(cert.issuer).stringify();
    const certificateSN = getSerialNumber(cert);
    await ejbcaFacade.revokeCertificate(issuerDN, certificateSN);
  }
}

async function throwAwayCertificate({ csr: csrPem }) {
  const csr = parseCSR(csrPem);

  const subjectDN = DN.from(csr.subject).stringify();

  const certificatePem = await ejbcaFacade.generateCertificate(
    subjectDN, certCfg.validity, csrPem,
  );

  const certificateFingerprint = getFingerprint(certificatePem);

  return { certificateFingerprint, certificatePem };
}

module.exports = {
  generateCertificate,
  registerCertificate,
  changeOwnership,
  getCertificate,
  listCertificates,
  deleteCertificate,
  throwAwayCertificate,
};
