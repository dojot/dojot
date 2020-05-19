const http = require('http');
const fs = require('fs');
const soap = require('soap');
const readline = require('readline');
const { logger } = require('@dojot/dojot-module-logger');
const { ejbca: ejbcaCfg } = require('../config');

const fsPromises = fs.promises;

/* SOAP client responsible for communicating with the EJBCA server */
let ejbcaClient = null;

/* flag to control whether the SOAP client is being created or not
 * (required due to asynchronous execution) */
let creatingEjbcaClient = false;

async function checkSecurityFiles() {
  try {
    await fsPromises.access(ejbcaCfg.pkcs12, fs.constants.F_OK);
  } catch (err) {
    logger.debug(`PKCS#12 binary file not found in: ${ejbcaCfg.pkcs12}`);
    throw err;
  }
  try {
    await fsPromises.access(ejbcaCfg.pkcs12secret, fs.constants.F_OK);
  } catch (err) {
    logger.debug(`PKCS#12 passphrase file not found in: ${ejbcaCfg.pkcs12secret}`);
    throw err;
  }
}

async function getFirstLine(pathToFile) {
  const readable = fs.createReadStream(pathToFile);
  const reader = readline.createInterface({ input: readable });
  const firstLine = await new Promise((resolve) => {
    reader.on('line', (line) => {
      reader.close();
      resolve(line);
    });
  });
  readable.close();
  return firstLine;
}

async function createSoapClient() {
  /* If there is already a SOAP client capable of communicating with
   * the EJBCA server, it is not necessary to create another one. */
  if (ejbcaClient) {
    return;
  }

  /* Checks whether the files used for authentication on the EJBCA
   * server actually exist, otherwise, an error is thrown */
  await checkSecurityFiles();

  /* loads PKCS#12 binary file (standard ASN.1 - DER encoding) */
  const p12Der = await fsPromises.readFile(ejbcaCfg.pkcs12);

  /* retrieves the PKCS#12 passphrase */
  const passphrase = await getFirstLine(ejbcaCfg.pkcs12secret);

  /* loads the trusted CA PEM encoded certificate */
  const trustedCA = await fsPromises.readFile(ejbcaCfg.trustedCA);

  /* Creates a SOAP client based on the WSDL provided by the EJBCA server */
  ejbcaClient = await soap.createClientAsync(ejbcaCfg.wsdl, {
    wsdl_options: {
      ca: trustedCA,
    },
  });

  /* Defines mutual authentication via TLS between the SOAP client and the EJBCA server */
  ejbcaClient.setSecurity(new soap.ClientSSLSecurityPFX(p12Der, passphrase));
}

async function createEjbcaClient() {
  /* If there is already a request in the EventLoop trying to establish
   * communication with the EJBCA server, it cancels the current request. */
  if (creatingEjbcaClient) {
    throw new Error('Certificate authority currently unavailable');
  }

  /* tries to establish communication with the EJBCA server... */
  try {
    creatingEjbcaClient = true;
    await createSoapClient();
  } catch (err) {
    ejbcaClient = null;
    logger.error(err);
    throw new Error('Failure to establish communication with the certification authority');
  } finally {
    creatingEjbcaClient = false;
  }
}

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

  /* EjbcaWS expects a timezone (zero UTC offset) in the following format:
   * YYYY-MM-DD HH:mm:ss+00:00
   */
  return `${YYYY}-${MM}-${DD} ${HH}:${mm}:${ss}+00:00`;
}

const facade = {

  async healthCheck() {
    const allOk = await new Promise((resolve) => {
      http.get(ejbcaCfg.healthCheck, (res) => {
        let data = '';
        /* A chunk of data has been recieved. */
        res.on('data', (chunk) => { data += chunk; });
        /* The whole response has been received. Print out the result. */
        res.on('end', () => {
          if (data === 'ALLOK') {
            resolve(true);
          } else {
            resolve(false);
          }
        });
      }).on('error', () => {
        resolve(false);
      });
    });
    return allOk;
  },

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

    /**
     * the EJBCA dummy user https://download.primekey.se/docs/EJBCA-Enterprise/latest/ws/org/ejbca/core/protocol/ws/client/gen/UserDataVOWS.html
     */
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

    /**
     * the PKCS10 request in base64
     */
    const requestData = csrPem;

    /**
     * | Constant Field       | Value |
     * | CERT_REQ_TYPE_PKCS10 | 0     |
     *
     * https://download.primekey.se/docs/EJBCA-Enterprise/latest/ws/org/ejbca/core/protocol/ws/common/CertificateHelper.html#CERT_REQ_TYPE_PKCS10
     */
    const requestType = 0;

    /**
     * | Constant Field           | Value         |
     * | RESPONSETYPE_CERTIFICATE | "CERTIFICATE" |
     *
     * https://download.primekey.se/docs/EJBCA-Enterprise/latest/ws/org/ejbca/core/protocol/ws/common/CertificateHelper.html#RESPONSETYPE_CERTIFICATE
     */
    const responseType = 'CERTIFICATE';

    /**
     * EJBCA SOAP Operation: certificateRequest
     *
     * Generates a certificate for a user. If the user is not already present in the database
     * it will be added otherwise it will be overwritten.
     * Status is automatically set to STATUS_NEW.
     *
     * Parameters
     * - userData     : the user
     * - requestData  : the PKCS10/CRMF/SPKAC/PUBLICKEY request in base64
     * - requestType  : PKCS10, CRMF, SPKAC or PUBLICKEY request as specified
     *                  by CertificateHelper.CERT_REQ_TYPE_ parameters.
     * - hardTokenSN  : Hard Token support was dropped since 7.1.0. Use null as this parameter
     * - responseType : indicating which type of answer that should be returned,
     *                  on of the CertificateHelper.RESPONSETYPE_ parameters.
     *
     * Doc URL:
     * https://download.primekey.se/docs/EJBCA-Enterprise/latest/ws/org/ejbca/core/protocol/ws/client/gen/EjbcaWS.html#certificateRequest(org.ejbca.core.protocol.ws.client.gen.UserDataVOWS,java.lang.String,int,java.lang.String,java.lang.String)
     */
    const args = {
      arg0: userData,
      arg1: requestData,
      arg2: requestType,
      arg3: null,
      arg4: responseType, /* return a BASE64 encoded certificate */
    };

    /* performs the operation of the SOAP web service and
     * retrieves the certificate x509 described in ASN1 and encoded in DER.
     * represented in Base64 so that it could be transferred via web service */
    const [result] = (await ejbcaClient.certificateRequestAsync(args));

    /* The EJBCA SOAP web service encodes the certificate in Base64, but stores
     * it in a byte[] instead of a String, so the Java-XML conversion lib has to
     * encode the byte[] again in Base64 and this time stores the result in a
     * String to be able to insert in the response xml. Because of this, it is
     * necessary to decode the response in Base64 in order to have access to the
     * certificate encoded in Base64. Somewhat confused and careless by the EJBCA
     * to encode the certificate in base64 twice... */
    const certBase64encoded = Buffer.from(result.return.data, 'base64').toString('utf-8');

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
  },

  async revokeCertificate(issuerDN, certificateSN) {
    /**
     * EJBCA SOAP Operation: revokeCert
     *
     * Revokes a user certificate.
     *
     * Parameters
     * - issuerDN      : The issuer field identifies the entity that has signed
     *                   and issued the certificate. The issuer field MUST contain
     *                   a non-empty distinguished name (DN).
     * - certificateSN : The serial number MUST be a positive integer assigned by
     *                   the CA to each certificate.
     *                   It MUST be unique for each certificate issued by a given CA.
     * - reason        : -1, "NOT_REVOKED",           "The Certificate Is Not Revoked"
     *                   0, "UNSPECIFIED",            "Unspecified"
     *                   1, "KEY_COMPROMISE",         "Key Compromise"
     *                   2, "CA_COMPROMISE",          "CA Compromise"
     *                   3, "AFFILIATION_CHANGED",    "Affiliation Changed"
     *                   4, "SUPERSEDED",             "Superseded"
     *                   5, "CESSATION_OF_OPERATION", "Cessation of Operation"
     *                   6, "CERTIFICATE_HOLD",       "Certificate Hold"
     *                   8, "REMOVE_FROM_CRL",        "Remove from CRL"
     *                   9, "PRIVILEGES_WITHDRAWN",   "Privileges Withdrawn"
     *                   10, "AA_COMPROMISE",         "AA Compromise"
     *
     * Doc URL:
     * https://download.primekey.se/docs/EJBCA-Enterprise/latest/ws/org/ejbca/core/protocol/ws/client/gen/EjbcaWS.html#revokeCert(java.lang.String,java.lang.String,int)
     */
    const args = {
      arg0: issuerDN,
      arg1: certificateSN,
      arg2: 4, /* Superseded */
    };
    await ejbcaClient.revokeCertAsync(args);
  },
};

const handler = {
  get(targetObj, property, receiver) {
    const value = Reflect.get(targetObj, property, receiver);
    if (typeof value === 'function') {
      if (value.proxy) {
        return value.proxy;
      }
      const proxy = new Proxy(value, {
        async apply(targetFunc, thisArgument, argumentsList) {
          if (!ejbcaClient) {
            await createEjbcaClient();
          }
          try {
            const result = await Reflect.apply(targetFunc, thisArgument, argumentsList);
            return result;
          } catch (err) {
            logger.error(err);
            throw new Error('Failure to perform operation on the certification authority');
          }
        },
      });
      value.proxy = proxy;
      return proxy;
    }
    return value;
  },
};

module.exports = new Proxy(facade, handler);
