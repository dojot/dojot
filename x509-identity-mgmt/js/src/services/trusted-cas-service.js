const ejbcaFacade = require('../core/ejbca-facade');

const { ejbca: ejbcaCfg } = require('../config');

const { getFingerprint } = require('../core/pki-utils');

async function getRootCertificate() {
  const certificatePem = await ejbcaFacade.getRootCertificate(ejbcaCfg.rootCA);

  const certificateFingerprint = getFingerprint(certificatePem);

  return { certificateFingerprint, certificatePem };
}

async function getRootCRL() {
  const crl = await ejbcaFacade.getCRL(ejbcaCfg.rootCA);
  return { crl };
}

module.exports = {
  getRootCertificate,
  getRootCRL,
};
