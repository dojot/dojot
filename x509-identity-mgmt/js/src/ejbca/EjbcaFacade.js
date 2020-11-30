/**
 * a string in simplified extended ISO format (ISO 8601)
 * @param {Date} date to convert
 */
function ejbcaTimezone(date) {
  const pad = (number) => {
    if (number < 10) {
      return `0${number}`;
    }
    return number;
  };

  const YYYY = date.getUTCFullYear();
  const MM = pad(date.getUTCMonth() + 1);
  const DD = pad(date.getUTCDate());
  const HH = pad(date.getUTCHours());
  const mm = pad(date.getUTCMinutes());
  const ss = pad(date.getUTCSeconds());

  // EjbcaWS expects a timezone (zero UTC offset) in the following format:
  // YYYY-MM-DD HH:mm:ss+00:00
  // This is only accepted in RFC-3339 (without T), different than ISO-8601.
  return `${YYYY}-${MM}-${DD} ${HH}:${mm}:${ss}+00:00`;
}

/**
 * A facade that serves as a front interface masking more complex EJBCA SOAP structural code
 */
class EjbcaFacade {
  /**
   * constructor function
   *
   * @param {Proxy} cradle is a proxy, and all getters will trigger a di-container.resolve().
   *                       The cradle is actually being passed to the constructor/factory function,
   *                       which is how everything gets wired up (Dependency Injection).
   */
  constructor({ ejbcaSoap, forceCRLRenew }) {
    Object.defineProperty(this, 'ejbcaSoap', { value: ejbcaSoap });
    Object.defineProperty(this, 'forceCRLRenew', { value: forceCRLRenew });
  }

  /**
   * Delegates to the Certificate Authority (EJBCA) the issue of a certificate to the informed CSR.
   *
   * @param {string} subjectDN to be included in the certificate to be generated.
   * @param {number} validity of the certificate to be generated.
   * @param {string} csrPem encoded with request for certificate signing.
   */
  async generateCertificate(subjectDN, validity, csrPem) {
    let startTime = null;
    let endTime = null;
    if (validity) {
      const days = Math.abs(parseInt(validity, 10));
      if (!Number.isNaN(days)) {
        const notBefore = new Date();
        const notAfter = new Date(notBefore);
        notAfter.setDate(notAfter.getDate() + days);

        startTime = ejbcaTimezone(notBefore);
        endTime = ejbcaTimezone(notAfter);
      }
    }

    // the EJBCA dummy user:
    // https://download.primekey.se/docs/EJBCA-Enterprise/latest/ws/org/ejbca/core/protocol/ws/client/gen/UserDataVOWS.html
    const userData = {
      username: 'X509Identity',
      certificateProfileName: 'X509Identity',
      endEntityProfileName: 'X509Identity',
      caName: 'X509 Identity CA',
      subjectDN,
      subjectAltName: '',
      keyRecoverable: false,
      sendNotification: false,
      startTime,
      endTime,
      // extendedInformation: {},
    };

    // the PKCS10 request in base64
    const requestData = csrPem;

    //
    // | Constant Field       | Value |
    // | CERT_REQ_TYPE_PKCS10 | 0     |
    //
    // https://download.primekey.se/docs/EJBCA-Enterprise/latest/ws/org/ejbca/core/protocol/ws/common/CertificateHelper.html#CERT_REQ_TYPE_PKCS10
    const requestType = 0;

    //
    // | Constant Field           | Value         |
    // | RESPONSETYPE_CERTIFICATE | "CERTIFICATE" |
    //
    // https://download.primekey.se/docs/EJBCA-Enterprise/latest/ws/org/ejbca/core/protocol/ws/common/CertificateHelper.html#RESPONSETYPE_CERTIFICATE
    const responseType = 'CERTIFICATE';

    //
    // EJBCA SOAP Operation: certificateRequest
    //
    // Generates a certificate for a user. If the user is not already present in the database
    // it will be added otherwise it will be overwritten.
    // Status is automatically set to STATUS_NEW.
    //
    // Parameters:
    // - userData     = the user
    // - requestData  = the PKCS10/CRMF/SPKAC/PUBLICKEY request in base64
    // - requestType  = PKCS10, CRMF, SPKAC or PUBLICKEY request as specified
    //                  by CertificateHelper.CERT_REQ_TYPE_ parameters.
    // - hardTokenSN  = Hard Token support was dropped since 7.1.0. Use null as this parameter
    // - responseType = indicating which type of answer that should be returned,
    //                  on of the CertificateHelper.RESPONSETYPE_ parameters.
    //
    // Doc URL:
    // https://download.primekey.se/docs/EJBCA-Enterprise/latest/ws/org/ejbca/core/protocol/ws/client/gen/EjbcaWS.html#certificateRequest(org.ejbca.core.protocol.ws.client.gen.UserDataVOWS,java.lang.String,int,java.lang.String,java.lang.String)
    const args = {
      arg0: userData,
      arg1: requestData,
      arg2: requestType,
      arg3: null,
      arg4: responseType, /* return a BASE64 encoded certificate */
    };

    // Retrieves the SOAP communication channel with the EJBCA
    const ejbcaClient = await this.ejbcaSoap.getClient();

    // performs the operation of the SOAP web service and
    // retrieves the certificate x509 described in ASN1 and encoded in DER.
    // represented in Base64 so that it could be transferred via web service
    const [result] = (await ejbcaClient.certificateRequestAsync(args));

    // The EJBCA SOAP web service encodes the certificate in Base64, but stores
    // it in a byte[] instead of a String, so the Java-XML conversion lib has to
    // encode the byte[] again in Base64 and this time stores the result in a
    // String to be able to insert in the response xml. Because of this, it is
    // necessary to decode the response in Base64 in order to have access to the
    // certificate encoded in Base64. Somewhat confused and careless by the EJBCA
    // to encode the certificate in base64 twice...
    const certBase64encoded = Buffer.from(result.return.data, 'base64').toString('utf-8');

    // PEM format is just the DER binary data that has been base64 encoded,
    // split into 64 character lines, and wrapped between:
    // '-----BEGIN CERTIFICATE-----' and '-----END CERTIFICATE-----'
    const certPemWrapped = [
      '-----BEGIN CERTIFICATE-----',
      certBase64encoded,
      '-----END CERTIFICATE-----',
    ].join('\n');

    // returns the certificate wrapped in PEM format
    return certPemWrapped;
  }

  /**
   * Revokes a certificate issued by the EJBCA. The serial number of the certificate
   * will be included in the next CRL that the EJBCA issues.
   *
   * @param {string} issuerDN of the CA that issued the certificate to be revoked.
   * @param {string} certificateSN Serial number of the certificate to be revoked.
   */
  async revokeCertificate(issuerDN, certificateSN) {
    //
    // EJBCA SOAP Operation: revokeCert
    //
    // Revokes a user certificate.
    //
    // Parameters:
    // - issuerDN      = The issuer field identifies the entity that has signed
    //                   and issued the certificate. The issuer field MUST contain
    //                   a non-empty distinguished name (DN).
    // - certificateSN = The serial number MUST be a positive integer assigned by
    //                   the CA to each certificate.
    //                   It MUST be unique for each certificate issued by a given CA.
    // - reason        = -1, "NOT_REVOKED",           "The Certificate Is Not Revoked"
    //                   0, "UNSPECIFIED",            "Unspecified"
    //                   1, "KEY_COMPROMISE",         "Key Compromise"
    //                   2, "CA_COMPROMISE",          "CA Compromise"
    //                   3, "AFFILIATION_CHANGED",    "Affiliation Changed"
    //                   4, "SUPERSEDED",             "Superseded"
    //                   5, "CESSATION_OF_OPERATION", "Cessation of Operation"
    //                   6, "CERTIFICATE_HOLD",       "Certificate Hold"
    //                   8, "REMOVE_FROM_CRL",        "Remove from CRL"
    //                   9, "PRIVILEGES_WITHDRAWN",   "Privileges Withdrawn"
    //                   10, "AA_COMPROMISE",         "AA Compromise"
    //
    // Doc URL:
    // https://download.primekey.se/docs/EJBCA-Enterprise/latest/ws/org/ejbca/core/protocol/ws/client/gen/EjbcaWS.html#revokeCert(java.lang.String,java.lang.String,int)
    const args = {
      arg0: issuerDN,
      arg1: certificateSN,
      arg2: 4, /* Superseded */
    };

    // Retrieves the SOAP communication channel with the EJBCA
    const ejbcaClient = await this.ejbcaSoap.getClient();

    // Revokes the certificate at the EJBCA
    await ejbcaClient.revokeCertAsync(args);
  }

  /**
   * Obtains the EJBCA root CA certificate
   *
   * @param {string} caName on the CA's registry within the EJBCA
   */
  async getRootCertificate(caName) {
    //
    // EJBCA SOAP Operation: getLastCAChain
    //
    // Retrieves the current certificate chain for a CA.
    //
    // Parameters:
    // - caname = the unique name of the CA whose certificate chain should be returned
    //
    // https://download.primekey.se/docs/EJBCA-Enterprise/latest/ws/org/ejbca/core/protocol/ws/client/gen/EjbcaWS.html#getLastCAChain(java.lang.String)
    const args = {
      arg0: caName,
    };

    // Retrieves the SOAP communication channel with the EJBCA
    const ejbcaClient = await this.ejbcaSoap.getClient();

    // performs the operation of the SOAP web service and
    // retrieves the certificate x509 described in ASN1 and encoded in DER.
    // represented in Base64 so that it could be transferred via web service
    const [result] = (await ejbcaClient.getLastCAChainAsync(args));

    // The EJBCA SOAP web service encodes the certificate in Base64, but stores
    // it in a byte[] instead of a String, so the Java-XML conversion lib has to
    // encode the byte[] again in Base64 and this time stores the result in a
    // String to be able to insert in the response xml. Because of this, it is
    // necessary to decode the response in Base64 in order to have access to the
    // certificate encoded in Base64. Somewhat confused and careless by the EJBCA
    // to encode the certificate in base64 twice... */
    const certBase64encoded = Buffer.from(result.return.pop().certificateData, 'base64').toString('utf-8');

    /* PEM format is just the DER binary data that has been base64 encoded,
     * split into 64 character lines, and wrapped between:
     * '-----BEGIN CERTIFICATE-----' and '-----END CERTIFICATE-----' */
    const certPemWrapped = [
      '-----BEGIN CERTIFICATE-----',
      certBase64encoded,
      '-----END CERTIFICATE-----',
    ].join('\n');

    /* returns the certificate wrapped in PEM format */
    return certPemWrapped;
  }

  /**
   * Get the latest (most current) CRL issued by the EJBCA.
   *
   * @param {string} caName of CA responsible for CRL.
   * @param {*} renew indicates whether a new CRL should be forced to be generated.
   * @param {*} deltaCRL indicates whether to obtain a delta or complete CRL.
   */
  async getCRL(caName, renew = false, deltaCRL = false) {
    // Retrieves the SOAP communication channel with the EJBCA
    const ejbcaClient = await this.ejbcaSoap.getClient();

    if (renew || this.forceCRLRenew) {
      // EJBCA SOAP Operation: createCRL
      //
      // Generates a CRL for the given CA.
      //
      // Parameters:
      // caname = the name in EJBCA of the CA that should have a new CRL generated
      //
      // https://download.primekey.se/docs/EJBCA-Enterprise/latest/ws/org/ejbca/core/protocol/ws/client/gen/EjbcaWS.html#createCRL(java.lang.String)
      const args1 = {
        arg0: caName,
      };
      await ejbcaClient.createCRLAsync(args1);
    }

    // EJBCA SOAP Operation: getLatestCRL
    //
    // Retrieves the latest CRL issued by the given CA.
    //
    // Parameters:
    // - caname   = the name in EJBCA of the CA that issued the desired CRL
    // - deltaCRL = false to fetch a full CRL, true to fetch a deltaCRL (if issued)
    //
    // https://download.primekey.se/docs/EJBCA-Enterprise/latest/ws/org/ejbca/core/protocol/ws/client/gen/EjbcaWS.html#getLatestCRL(java.lang.String,boolean)
    const args2 = {
      arg0: caName,
      arg1: deltaCRL,
    };

    // performs the operation of the SOAP web service and
    // retrieves the CRL described in ASN1 and encoded in DER.
    // represented in Base64 so that it could be transferred via web service
    const [result] = await ejbcaClient.getLatestCRLAsync(args2);
    const crlBase64encoded = result.return.match(/.{1,64}/g).join('\n');

    // PEM format is just the DER binary data that has been base64 encoded,
    // split into 64 character lines, and wrapped between:
    // '-----BEGIN X509 CRL-----' and '-----END X509 CRL-----'
    const crlPemWrapped = [
      '-----BEGIN X509 CRL-----',
      crlBase64encoded,
      '-----END X509 CRL-----',
    ].join('\n');

    // returns the CRL wrapped in PEM format
    return crlPemWrapped;
  }
}

module.exports = EjbcaFacade;
