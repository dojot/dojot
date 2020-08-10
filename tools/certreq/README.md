# CertReq

This is a scripting tool for requesting certificates from the dojot platform. It is responsible for retrieving the Root CA certificate from the platform, creating the CSR (Certificate signing request) and requesting the signature of the certificate for a specific entity (usually being a device).

To establish encrypted communication between the client and the server, it is necessary to use the TLS protocol and x.509 certificates. However, for the client to be able to publish data on the platform, it must be identified through a certificate. Therefore, the TLS must be established in a mutual way, that is, both the client and the platform exchange certificates during the TLS handshake.

Basically the functionality of this tool is to connect to the API platform (using a __username__ and __password__) and request certificates according to the tanant to which the user belongs. Each certificate will have an __identifier__ associated with a device on the dojot platform, so for the certificate to be useful, it is necessary to inform valid identifiers (device IDs) according to the user's tenant. The tenant is automatically associated in generating the certificate.

With this tool, simply inform the list of device identifiers separated by commas and the tool will retrieve the respective certificates. If the identifier does not belong to any tentant device, the certificate will be of no use, because despite the TLS connection being established, the platform will not let anything be published.

It uses:
- [OpenSSL](https://www.openssl.org/) to generate CSR and cryptographic keys;
- [curl](https://curl.haxx.se/) to access the dojot platform APIs;
- [jq](https://stedolan.github.io/jq/) to process the return JSON from the dojot platform APIs;
- [GNU awk](https://www.gnu.org/software/gawk/) to extract data from certain parts of the output of some commands in the shell.

## Usage

This tool creates the directories `./ca` and `./cert_{identifier}` to store the certificates and public/private keys.

###### Example 1

Give the script execution permission (`chmod u+x certreq.sh`) and enter the command:

~~~bash
./certreq.sh
~~~

> In this example, no parameters were entered, so all of then will assume the default value.

That done, The script will retrieve the root CA certificate from the dojot platform and store it in the `./ca` directory.<br>
As no identifier was entered as a parameter, the script will retrieve the identifiers for all devices that the user has access to and a certificate will be generated for each device.

The certificate and key pair will be saved in the `./cert_{identifier}` directory.

###### Example 2

The following example explores all the parameters that can be entered for the script:

~~~bash
./certreq.sh \
    -h localhost \
    -p 8000 \
    -i '123,456,789' \
    -u 'admin' \
    -s 'admin'
~~~

Given a _username_ `admin` and _password_ `admin`, this command will request 3 certificates with _identifiers_ `123`, `456` and `789` for the dojot platform _host_ `localhost` on _port_ `8000`.

#### Parameters

key | value
--- | -----
-h  | Hostname (or IP) of the dojot platform.<br>**Default**: 127.0.0.1
-p  | Port on which the dojot platform responds to the certificate issuing API.<br>**Default**: 8000
-i  | Entity identifier. It must have the same device ID registered on the dojot platform. If not informed, a certificate will be generated _for each device_ that the user has access to.
-u  | Username to access the _x509-identity-mgmt_ API for issuing certificates.<br>**Default**: admin
-s  | User password (secret)  to access the _x509-identity-mgmt_ API for issuing certificates.<br>**Default**: admin

## Lastly

With the CA certificate and the device's certificate and key pair, it is possible to publish data on the dojot platform (over mutual TLS). For example, through the MqTT protocol, using the command `mosquitto_pub`:

~~~bash
mosquitto_pub -m '{"timestamp":654321}' -t 'admin:123456/attrs' -h 'dojot.host.com' -p 8883 --cafile './ca/ca.pem' --cert './cert_123456/cert.pem' --key './cert_123456/private.key' -d
~~~
> The details of the `mosquitto_pub` can be found in its [manual](https://mosquitto.org/man/mosquitto_pub-1.html).
>