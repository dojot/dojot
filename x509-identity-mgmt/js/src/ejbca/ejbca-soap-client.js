const fs = require('fs');
const soap = require('soap');
const readline = require('readline');

async function checkSecurityFiles({ pkcs12, pkcs12secret, logger }) {
  try {
    await fs.promises.access(pkcs12, fs.constants.F_OK);
  } catch (err) {
    logger.debug(`PKCS#12 binary file not found in: ${pkcs12}`);
    throw err;
  }
  try {
    await fs.promises.access(pkcs12secret, fs.constants.F_OK);
  } catch (err) {
    logger.debug(`PKCS#12 passphrase file not found in: ${pkcs12secret}`);
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

async function createClient({
  wsdl, pkcs12, pkcs12secret, trustedCA,
}) {
  // loads PKCS#12 binary file (standard ASN.1 - DER encoding)
  const p12Der = await fs.promises.readFile(pkcs12);

  // retrieves the PKCS#12 passphrase
  const passphrase = await getFirstLine(pkcs12secret);

  // loads the trusted CA PEM encoded certificate
  const ca = await fs.promises.readFile(trustedCA);

  // Creates a SOAP client based on the WSDL provided by the EJBCA server
  const client = await soap.createClientAsync(wsdl, {
    wsdl_options: { ca },
  });

  // Defines mutual authentication via TLS between the SOAP client and the EJBCA server
  client.setSecurity(new soap.ClientSSLSecurityPFX(p12Der, passphrase));

  return client;
}

class EjbcaSoap {
  constructor({
    wsdl, pkcs12, pkcs12secret, trustedCA, logger,
  }) {
    Object.defineProperty(this, 'wsdl', { value: wsdl });
    Object.defineProperty(this, 'pkcs12', { value: pkcs12 });
    Object.defineProperty(this, 'pkcs12secret', { value: pkcs12secret });
    Object.defineProperty(this, 'trustedCA', { value: trustedCA });
    Object.defineProperty(this, 'logger', { value: logger });

    // SOAP client responsible for communicating with the EJBCA server
    Object.defineProperty(this, 'client', { value: null, writable: true });

    // State to control whether the SOAP client is being created or not
    // (required due to asynchronous execution).
    Object.defineProperty(this, 'inactive', { value: false, writable: true });
  }

  async getClient() {
    // If there is already a request in the EventLoop trying to establish
    // communication with the EJBCA server, it cancels the current request.
    if (this.inactive) {
      // sleeps for 5 seconds to allow time to establish communication with
      // the EJBCA server and create a channel for the remote procedure call
      await new Promise((resolve) => setTimeout(resolve, 1000 * 5));

      // If after letting the EventLoop run for 5 seconds and communication
      // with the EJBCA server has not yet been established, an error returns.
      if (this.inactive) {
        throw new Error('Certificate authority currently unavailable');
      }
    }

    // If there is already a SOAP client capable of communicating with
    // the EJBCA server, it is not necessary to create another one.
    if (this.client) {
      return this.client;
    }

    try {
      // makes this object temporarily inactive for interested parties waiting
      // in the EventLoop, it remains inactive until a SOAP client is created...
      this.inactive = true;

      // Checks whether the files used for authentication on the EJBCA
      // server actually exist, otherwise, an error is thrown
      await checkSecurityFiles(this);

      // tries to establish communication with the EJBCA server to create a SOAP client...
      this.client = await createClient(this);

      return this.client;
    } catch (err) {
      // If something went wrong, we must ensure that the SOAP client was not created
      this.client = null;
      this.logger.error(err);
      throw new Error('Failure to establish communication with the certification authority');
    } finally {
      // Finally, we made this object active again to those interested waiting
      // in the eventloop, as the creation process of the SOAP client was
      // completed, regardless of success or failure.
      this.inactive = false;
    }
  }
}

module.exports = EjbcaSoap;
