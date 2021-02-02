const asn1js = require('asn1js');

const pkijs = require('pkijs');

const crypto = require('crypto');

const { BadRequest } = require('./errors');

const { certificate: certCfg } = require('../config');

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

/**
 * Retrieves the DER structure represented in base64.
 *
 * @param {*} pem A PEM-encoded DER (ASN.1 data structure)
 * @return {String} DER in base64.
 */
function getDER(pem) {
  /* remove PEM header/footer and all non-base64 characters */
  return pem.replace(/(^-.+?-.+$|[^A-Za-z0-9+/=])/gm, '');
}

/**
 * Converts a Buffer (Node.js) to an ArrayBuffer (javascript standard built-in object)
 * @param {Buffer} buf (https://nodejs.org/api/buffer.html#buffer_class_buffer)
 * @return {ArrayBuffer} (https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer)
 */
function getArraybuffer(buf) {
  /* You may use the `byteLength` property instead of the `length` one. */
  return buf.buffer.slice(buf.byteOffset, buf.byteOffset + buf.length);
}

/**
 * Converts a javascript ArrayBuffer to a hex string array
 * @param {*} arrayBuffer (https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer)
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
 * @param {*} pem pem A PEM-encoded x.509 Certificate or CSR.
 * @return {pkijs.CertificationRequest} an ASN1 data structure.
 */
function getASN1(pem) {
  const derBase64 = getDER(pem);

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
 * @param {String} pem A PEM-encoded certificate
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
 * @param {pkijs.PublicKeyInfo} Destructuring only attributes "algorithm" and "parsedKey"
 */
function checkPublicKey({ algorithm, parsedKey }) {
  if (!certCfg.checkPublicKey) {
    return;
  }
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
}

/**
 * Calculates the fingerprint of the certificate using the SHA256
 * hashing algorithm. The certificate must be in PEM format.
 *
 * @param {String} pem A PEM-encoded certificate
 * @return {string} The Certificate fingerprint (256 bits) represented in hexadecimal.
 */
function getFingerprint(pem) {
  const hash = crypto.createHash('sha256');
  hash.update(getDER(pem), 'base64');

  /* perform a SHA256 hash on it */
  const fingerprint = hash.digest('hex')
    .match(/.{2}/g)
    .join(':')
    .toUpperCase();

  return fingerprint;
}

function getSerialNumber(certificate) {
  const arrHex = arrayBufferToHex(certificate.serialNumber.valueBlock.valueHex);
  return arrHex.join('').toUpperCase();
}

module.exports = {
  parseCSR,
  parseCert,
  checkPublicKey,
  getFingerprint,
  getSerialNumber,
};
