const asn1js = require('asn1js');

const pkijs = require('pkijs');

const crypto = require('crypto');

const TOTAL_MILLISECONDS_IN_A_DAY = 1000 * 60 * 60 * 24;

/* Identification of key algorithms. For details:
 * http://oid-info.com/get/<OID-dot-notation> */
const RSA = '1.2.840.113549.1.1.1';
const ECDSA = '1.2.840.10045.2.1';

/* https://tools.ietf.org/html/rfc5480#section-2.1.1.1 */
const namedCurves = [
  '1.2.840.10045.3.1.7', // P-256
  '1.3.132.0.34', // P-384
  '1.3.132.0.35', // P-521
];

function createObject(logger, errorTemplate) {
  const { BadRequest } = errorTemplate;

  /**
 * Retrieves the DER structure represented in base64.
 *
 * @param {string} pem A PEM-encoded DER (ASN.1 data structure).
 * @return {String} DER in base64.
 */
  function getBase64DER(pem) {
  /* remove PEM header/footer and all non-base64 characters */
    return pem.replace(/(^-.+?-.+$|[^A-Za-z0-9+/=])/gm, '');
  }

  /**
 * Converts a Buffer (Node.js) to an ArrayBuffer (javascript standard built-in object).
 *
 * @param {Buffer} buf (https://nodejs.org/api/buffer.html#buffer_class_buffer).
 * @return {ArrayBuffer} (https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer).
 */
  function getArraybuffer(buf) {
  /* You may use the `byteLength` property instead of the `length` one. */
    return buf.buffer.slice(buf.byteOffset, buf.byteOffset + buf.length);
  }

  /**
 * Converts a javascript ArrayBuffer to a hex string array
 *
 * @param {ArrayBuffer} arrayBuffer (https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer).
 * @return {Array} a hex string array.
 */
  function arrayBufferToHex(arrayBuffer) {
  /* An Array is created from a Uint8Array holding the buffer data.
     * All the Uint8Array items (8-bit unsigned integers [0-255]) are mapped
     * to their hexadecimal representation and padded with 0 characters. */
    return [...new Uint8Array(arrayBuffer)].map(
      (uInt) => uInt.toString(16).padStart(2, '0'),
    );
  }

  /**
 * Obtains an ASN1 structure from a PEM encoded file.
 *
 * @param {string} pem pem A PEM-encoded x.509 Certificate or CSR.
 * @return {pkijs.CertificationRequest} an ASN1 data structure.
 *
 * @throws an exception if an ASN1 data structure cannot be obtained.
 */
  function getASN1(pem) {
    const derBase64 = getBase64DER(pem);

    /* decodes a base64-encoded DER to a ArrayBuffer */
    const der = getArraybuffer(Buffer.from(derBase64, 'base64'));

    /* creates an ASN1 data structure from the binary DER (a subset of BER) */
    const asn1Data = asn1js.fromBER(der);
    if (asn1Data.offset === (-1)) {
      throw BadRequest('Can not parse ASN1 data structure from binary data');
    }

    return asn1Data;
  }

  /**
 * parses a string argument and returns a PKCS#10 object.
 *
 * @param {String} pem A PEM-encoded Certificate Signing Request.
 * @return {pkijs.CertificationRequest} an object PKCS#10.
 */
  function parseCSR(pem) {
    const asn1Data = getASN1(pem);

    /* creates an object PKCS#10 (from RFC2986) */
    const csr = new pkijs.CertificationRequest({ schema: asn1Data.result });

    return csr;
  }

  /**
 * parses a string argument and returns a x.509 v3 Certificate.
 *
 * @param {String} pem A PEM-encoded certificate.
 * @return {pkijs.Certificate} a x.509 v3 Certificate.
 */
  function parseCert(pem) {
    const asn1Data = getASN1(pem);

    /* creates an x.509v3 certificate (from RFC5280) */
    const cert = new pkijs.Certificate({ schema: asn1Data.result });

    return cert;
  }

  /**
 * Checks whether the public key is supported by the application.
 *
 * @param {pkijs.PublicKeyInfo} Destructuring only attributes "algorithm" and "parsedKey".
 *
 * @throws an exception if the public key is not supported by the platform.
 */
  function checkPublicKey({ algorithm, parsedKey }) {
    const algoID = (algorithm) ? algorithm.algorithmId : '';
    if (algoID === RSA) {
      const keyLength = (parsedKey) ? parsedKey.modulus.valueBlock.valueHex.byteLength : 0;
      // 256 bytes == 2048 bits
      if (keyLength < 256) {
      // NIST recommends 2048-bit keys for RSA:
      // https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-57Pt3r1.pdf (page 12)
        throw BadRequest('The RSA public key length less than 2048 bits is not supported.');
      }
    } else if (algoID === ECDSA) {
      if (!namedCurves.includes(parsedKey && parsedKey.namedCurve)) {
      /* https://csrc.nist.gov/publications/detail/fips/186/4/final */
        throw BadRequest('The elliptical curve used in the ECDSA public key algorithm is not supported.'
                + ' (Use the NIST-recommended curves: P-256, P-384 or P-521)');
      }
    } else {
      const suffix = (algoID) ? ` The key algorithm with OID=${algoID} is not supported` : '';
      throw BadRequest(`The public key algorithm must be RSA or ECDSA.${suffix}`);
    }
    logger.debug('The public key is supported.');
  }

  /**
 * Calculates the fingerprint of the certificate using the SHA256
 * hashing algorithm. The certificate must be in PEM format.
 *
 * @param {String} pem A PEM-encoded certificate.
 * @return {string} The Certificate fingerprint (256 bits) represented in hexadecimal.
 */
  function getFingerprint(pem) {
    const hash = crypto.createHash('sha256');
    hash.update(getBase64DER(pem), 'base64');

    /* perform a SHA256 hash on it */
    const fingerprint = hash.digest('hex')
      .match(/.{2}/g)
      .join(':')
      .toUpperCase();

    return fingerprint;
  }

  /**
 * Obtain the serial number of the informed certificate.
 *
 * @param {pkijs.Certificate} certificate in which the serial number will be retrieved.
 *
 * @return {string} returns the serial number of the certificate.
 */
  function getSerialNumber(certificate) {
    const arrHex = arrayBufferToHex(certificate.serialNumber.valueBlock.valueHex);
    return arrHex.join('').toUpperCase();
  }

  /**
 * Checks the remaining days of validity of the informed certificate.
 *
 * @param {pkijs.Certificate} certificate to be verified.
 * @param {number} minimumValidityDays that the certificate still needs to have.
 *
 * @throws an exception if the certificate has expired.
 * @throws an exception if the certificate does not have a minimum validity days.
 */
  function checkRemainingDays(certificate, minimumValidityDays = 0) {
    const difference = certificate.notAfter.value - new Date();
    const remainingDays = Math.floor(difference / TOTAL_MILLISECONDS_IN_A_DAY);
    if (remainingDays < 0) {
      throw BadRequest(`The certificate expired ${Math.abs(remainingDays)} days ago.`);
    }
    if (remainingDays < minimumValidityDays) {
      throw BadRequest(`The certificate must be valid for more than ${minimumValidityDays} days.`);
    }
    logger.debug(`Certificate remaining days: ${remainingDays}`);
  }

  /**
 * assert that the certificate is the low end of the chain.
 *
 * @param {pkijs.Certificate} certificate to be verified.
 *
 * @throws an exception if the certificate is not the low end of the chain (or self signed).
 */
  async function assertLeaf(certificate) {
  // OID = 2.5.29.19
  // Description = Basic Constraints
    const extension = certificate.extensions.find((ext) => ext.extnID === '2.5.29.19');
    if (extension && extension.parsedValue && extension.parsedValue.cA) {
      throw BadRequest("The certificate is a CA certificate. It contains the flag 'CA:TRUE' in the extension 'Basic Constraints'.");
    }

    const certChainVerificationEngine = new pkijs.CertificateChainValidationEngine({
      trustedCerts: [certificate],
      certs: [certificate],
    });
    const { result: selfSigned } = await certChainVerificationEngine.verify();
    if (selfSigned) {
      throw BadRequest('The certificate is self-signed.');
    }
    logger.debug('The certificate is the low end of the chain.');
  }

  /**
 * assert that the certificate belongs to a root CA.
 *
 * @param {pkijs.Certificate} certificate to be verified.
 *
 * @throws an exception if the certificate does not belong to a root CA.
 */
  async function assertRootCA(certificate) {
  // OID = 2.5.29.19
  // Description = Basic Constraints
    const extension = certificate.extensions.find((ext) => ext.extnID === '2.5.29.19');
    if (!extension || !extension.parsedValue) {
      throw BadRequest("The certificate is not a CA certificate. It does not contain the extension 'Basic Constraints' (OID: 2.5.29.19).");
    }
    const { cA } = extension.parsedValue;
    if (!cA) {
      throw BadRequest("The certificate is not a CA certificate. It does not contain the flag 'CA:TRUE' in the extension 'Basic Constraints'.");
    }

    const certChainVerificationEngine = new pkijs.CertificateChainValidationEngine({
      trustedCerts: [certificate],
      certs: [certificate],
    });
    const { result: selfSigned } = await certChainVerificationEngine.verify();
    if (!selfSigned) {
      throw BadRequest('The certificate is not a Root CA certificate (self signed).');
    }
    logger.debug('The certificate belongs to a root CA.');
  }

  /**
 * checks whether the certificate belongs to a root CA.
 *
 * @param {pkijs.Certificate} certificate to be verified.
 *
 * @returns true if the certificate belongs to a root CA, otherwise, false.
 */
  async function isRootCA(certificate) {
    try {
      await assertRootCA(certificate);
    } catch (ex) {
      return false;
    }
    return true;
  }

  /**
 * Checks the certificate chain of trust up to their root CA.
 *
 * @param {List} chain of certificates to be checked.
 * @param {pkijs.Certificate} trusted CA certificate.
 *
 * @throws an exception if the chain of trust is not validated.
 */
  async function checkChainOfTrust(chain, trusted) {
    const certChainVerificationEngine = new pkijs.CertificateChainValidationEngine({
      trustedCerts: [trusted],
      certs: [...chain],
    });
    const { result } = await certChainVerificationEngine.verify();
    if (!result) {
      throw BadRequest('The certificate chain of trust is not valid.');
    }
    logger.debug('The certificate chain of trust is valid.');
  }

  /**
 * Checks that the external certificate does not have the same
 * CommonName as the platform's internal root CA.
 *
 * @param {pkijs.Certificate} certificate to be verified
 * @param {*} rootCN CommonName of the internal root CA of the platform.
 *
 * @throws an exception if the external certificate CN is the same as the
 *         internal root CA certificate CN of the platform.
 */
  function checkRootExternalCN(certificate, rootCN) {
    const cnameAttr = certificate.subject.typesAndValues.find((attr) => attr.type === '2.5.4.3');
    if (cnameAttr && cnameAttr.value && cnameAttr.value.valueBlock
      && cnameAttr.value.valueBlock.value === rootCN) {
      throw BadRequest("It is not allowed to register a CA certificate with the same 'CNAME' of the platform.");
    }
  }

  return Reflect.construct(function PKIUtils() {
    this.parseCSR = parseCSR;
    this.parseCert = parseCert;
    this.checkPublicKey = checkPublicKey;
    this.getFingerprint = getFingerprint;
    this.getSerialNumber = getSerialNumber;
    this.checkRemainingDays = checkRemainingDays;
    this.assertLeaf = assertLeaf;
    this.assertRootCA = assertRootCA;
    this.isRootCA = isRootCA;
    this.checkChainOfTrust = checkChainOfTrust;
    this.checkRootExternalCN = checkRootExternalCN;
  }, []);
}

module.exports = ({ logger, errorTemplate }) => createObject(logger, errorTemplate);
