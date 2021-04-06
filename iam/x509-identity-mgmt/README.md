# X.509 Identity Management

The purpose of this component is to provide x.509 identification for final dojot
entities, that is, *IoT devices* that communicate with the dojot IoT platform.

## Table of Contents

- [X.509 Identity Management](#x509-identity-management)
  - [Table of Contents](#table-of-contents)
  - [Overview](#overview)
  - [Component architecture](#component-architecture)
  - [Usage](#usage)
    - [How to obtain a certificate](#how-to-obtain-a-certificate)
      - [Generate a CSR](#generate-a-csr)
        - [Elliptical curve encryption](#elliptical-curve-encryption)
          - [P-256](#p-256)
          - [P-384](#p-384)
          - [P-521](#p-521)
      - [Submit the CSR to the dojot platform](#submit-the-csr-to-the-dojot-platform)
      - [Obtain the certificate issued by the dojot platform](#obtain-the-certificate-issued-by-the-dojot-platform)
      - [How to calculate the certificate _fingerprint_](#how-to-calculate-the-certificate-fingerprint)
    - [How to obtain the internal CA certificate of the dojot platform](#how-to-obtain-the-internal-ca-certificate-of-the-dojot-platform)
    - [How to list certificates](#how-to-list-certificates)
        - [The first and simplest](#the-first-and-simplest)
        - [The second option](#the-second-option)
    - [How to revoke a certificate](#how-to-revoke-a-certificate)
    - [How to register a trusted CA certificate](#how-to-register-a-trusted-ca-certificate)
    - [How to remove a trusted CA certificate](#how-to-remove-a-trusted-ca-certificate)
    - [How to list trusted CA certificates](#how-to-list-trusted-ca-certificates)
    - [How to enable/disable self-registration of certificates issued by a trusted CA](#how-to-enabledisable-self-registration-of-certificates-issued-by-a-trusted-ca)
    - [How to register an external certificate (issued by a trusted CA)](#how-to-register-an-external-certificate-issued-by-a-trusted-ca)
    - [How to remove an external certificate](#how-to-remove-an-external-certificate)
    - [How to associate a device with a certificate](#how-to-associate-a-device-with-a-certificate)
    - [How to unassociate a device with a certificate](#how-to-unassociate-a-device-with-a-certificate)
  - [Running the service](#running-the-service)
    - [Configurations](#configurations)
      - [HTTP Server settings](#http-server-settings)
      - [Express Framework settings](#express-framework-settings)
      - [EJBCA Integration settings](#ejbca-integration-settings)
      - [Certificate Control Settings](#certificate-control-settings)
      - [MongoDBClient/mongoose Settings](#mongodbclientmongoose-settings)
      - [Logger Settings](#logger-settings)
      - [Kafka Integration Settings](#kafka-integration-settings)
      - [_Device Manager_ Integration Settings](#device-manager-integration-settings)
      - [Certificate Service Settings](#certificate-service-settings)
    - [How to run](#how-to-run)
  - [Debugging the service](#debugging-the-service)
  - [Testing the service](#testing-the-service)
  - [Documentation](#documentation)
  - [Issues and help](#issues-and-help)

## Overview

X.509 certificates represent the identity of the IoT device for the dojot platform.
Such certificates are installed with the devices and presented by them when
establishing communication over TLS with the platform.
The platform keeps track of the device associated with an X.509 certificate.
Therefore, although it is possible to issue a certificate without necessarily
associating it with a device right away, so that this device can actually send
and receive data from the platform, there needs to be a *Certificate-Device*
relationship and the *X.509 Identity Management* component provides the
interfaces for this.

Certificates issued by the dojot platform are signed by an internal CA, so it is
also necessary for the device to recognize the dojot CA as being trusted. In a
simplified way, the device must have installed the certificate issued to it and
the certificate of the CA that signed it. The details are part of the
[TLS protocol](https://tools.ietf.org/html/rfc5246).

It is important that you see the [README](./../ejbca/README.md) of the EJBCA Server
custom settings for the dojot platform.


## Component architecture

The component is written in javascript and runs on the Node.js interpreter.
It makes use of the [EJBCA server](https://www.ejbca.org/) for issuing
certificates, which in turn needs to be connected to a
[Postgres](https://www.postgresql.org/) database to store its settings
centrally. Both applications run within the [Docker](https://www.docker.com/)
container and together form the **x509-identity-mgmt** service.
The component stores the certificates and their association with the devices
into [MongoDB](https://www.mongodb.com/).<br>
It is located behind the [API Gateway](https://github.com/dojot/kong) of the
dojot platform.


## Usage

### How to obtain a certificate

To obtain a certificate it is necessary to inform the CSR for the dojot platform.
CSR (Certificate Signing Request) is a text file generated by the user
(owner of the device) containing the information to request your certificate
securely, being used to generate a digitally signed certificate.

#### Generate a CSR

The user is responsible for generating a CSR, and for that, (s)he needs tools for
this, one of which (and perhaps the most common) is
[OpenSSL](https://www.openssl.org/). In this example we will use this tool to
generate a CSR on the user's computer.

With OpenSSL installed on the user's computer (Linux in this case), just type
the command below in the terminal:

~~~bash
openssl req \
  -newkey 'rsa:2048' \
  -nodes \
  -sha256 \
  -keyout 'private.key' \
  -out 'request.csr' \
  -subj '/CN={device-id here}'
~~~

Explaining this command in detail:

| command snippet                            | description |
|--------------------------------------------|-------------|
| openssl req                                | PKCS#10 certificate request utility |
| -newkey rsa:2048                           | creates a new CSR and a new private key. The argument **rsa:*nbits*** means that the algorithm used for the generation of the key pair will be RSA, where nbits is the size of the keys, in this case, the minimum accepted value for RSA is 2048 bits.|
| -nodes                                     | If this option is specified then if a private key is created it will not be encrypted (a.k.a. No DES). |
| -sha256                                    | This specifies the message digest to sign the request. |
| -keyout private.key                        | This gives the filename to write the newly created private key to. |
| -out request.csr                           | This specifies the output filename to write the CSR to |
| -subj "/CN=`{device-id here}`" | Sets subject name for new request. Only the CN (CommonName) is required. This field can contain any value. In the past, this field was used to link the `tenant:device-id` to the certificate, but this behavior has been disabled to support external certificates. |

For more details, just consult the tool manual:

~~~bash
man openssl-req
~~~

Upon completion of the command, two files will be generated, one of which is the
`private.key` and *must be stored with the device in a very secure way*. The
other is the CSR `request.csr` and it must be sent to the dojot platform in
order to obtain an x.509 certificate.

In this example, we saw how to generate a CSR whose key pair was derived from
the RSA algorithm. This algorithm, in order to be considered secure, needs to
generate keys of at least 2048 bits (really very large keys).
For IoT devices the ideal is to use pairs of keys derived from elliptical curves
(smaller keys, but with the same level of security). The dojot platform
currently supports the curves recommended by [NIST](https://www.nist.gov/),
namely: P-256, P-384 or P-521.

##### Elliptical curve encryption

Below are the commands needed to generate each of these curves:

###### P-256
~~~bash
openssl ecparam -genkey -name 'prime256v1' -noout -out 'ec256-key-pair.pem'

openssl req -new -key 'ec256-key-pair.pem' -out 'ec256-key-pair.csr' -sha256 -subj '/CN={device ID}'
~~~
###### P-384
~~~bash
openssl ecparam -genkey -name 'secp384r1' -noout -out 'ec384-key-pair.pem'

openssl req -new -key 'ec384-key-pair.pem' -out 'ec384-key-pair.csr' -sha256 -subj '/CN={device ID}'
~~~

###### P-521
~~~bash
openssl ecparam -genkey -name 'secp521r1' -noout -out 'ec512-key-pair.pem'

openssl req -new -key 'ec512-key-pair.pem' -out 'ec512-key-pair.csr' -sha256 -subj '/CN={device ID}'
~~~

#### Submit the CSR to the dojot platform

CSR submission must be done at endpoint `/api/v1/certificates` of the
*X.509 Identity Management* component, but as in practice the component is
behind an API Gateway, the URI becomes `http://{host}:{port}/x509/v1/certificates`.
Therefore, you must send the CSR according to the syntax:

~~~HTTP
POST <base-url>/v1/certificates
Authorization: Bearer JWT
Content-type: application/json

{
  "csr": <string>
}
~~~

Now the details come in, as we can see, the CSR generated by OpenSSL is nothing
more than a text file, but it has the following format:

    -----BEGIN CERTIFICATE REQUEST-----
    MIICVjCCAT4CAQAwETEPMA0GA1UEAwwGMTIzNDU2MIIBIjANBgkqhkiG9w0BAQEF
    AAOCAQ8AMIIBCgKCAQEAxbuzcHie/XjSsE5FN9GcXdfZbAAZDWAFyQ6n8q7Lk+vH
    nkd5xs9LskKRBEkLNDm/KOtiV7y9A55wGuTCvMrfqfU4pbSRDkF0zG66LxSZw0fO
    Qa2RUBG8gisq8/VkEw8AhKr9+tnZ5HMCwXdgzmw/Dr0YP1M7laEgO7ZbEM2gEs6S
    LVCRQrhisx2tLW907a4YM+EPVj14DjnSrC4OqaLshkhGlysiIQHKy1I75znEoyW6
    OOvsuYk3pluZJt4DXWyDVZbghtx1UO82aS6GmL+OO1lLxTzDEnkQGDUc9kKATrfL
    HqH3MFb7q3HLZrS+VhZWDQh/ng5HRR1Mt8XVa7Im0wIDAQABoAAwDQYJKoZIhvcN
    AQELBQADggEBAK41QA2ACotztfGAP18rG0iSOWdxoFlPGB/Qra9owNCaM60my58K
    Y8PA8WlY/Nfmke/0ZM1Qed3VGM9SOWmIOSnKSapNMnnMQ5wg1ebetPxm6RU6kMJw
    sqZ7n8xENm5/TXEfz7lg1/djU9yfMZAPs0bK8430oFfchx1kPMpM8w3fcOKgo2ii
    rc7HwKVbyQTstkuPiNEQzlFM2JtqTuqDDt+Mv1cA2yRrW+LtjuWfuzu1nMjA7F1j
    9GWtb/7Jta18oGNfHGZF3nPpRj9FkZJWYwFNq+O95Qsq1I6XWCwTfeFJF8BQYEBK
    Arduy9FzNG50zzJX0yYI/blzBL/EA27Vvr4=
    -----END CERTIFICATE REQUEST-----

Note that this format consists of a header and a footer, the CSR is encoded in
*Base64* in the middle. Also note that each line has a maximum of 64 characters.
As we have to send this file in a JSON payload, it is necessary to represent the
line breaks with the `\r\n` (or just `\n`) markers.
That done, we will have the following payload:

~~~javascript
{
  "csr": "-----BEGIN CERTIFICATE REQUEST-----\r\nMIICVjCCAT4CAQAwETEPMA0GA1UEAwwGMTIzNDU2MIIBIjANBgkqhkiG9w0BAQEF\r\nAAOCAQ8AMIIBCgKCAQEAxbuzcHie/XjSsE5FN9GcXdfZbAAZDWAFyQ6n8q7Lk+vH\r\nnkd5xs9LskKRBEkLNDm/KOtiV7y9A55wGuTCvMrfqfU4pbSRDkF0zG66LxSZw0fO\r\nQa2RUBG8gisq8/VkEw8AhKr9+tnZ5HMCwXdgzmw/Dr0YP1M7laEgO7ZbEM2gEs6S\r\nLVCRQrhisx2tLW907a4YM+EPVj14DjnSrC4OqaLshkhGlysiIQHKy1I75znEoyW6\r\nOOvsuYk3pluZJt4DXWyDVZbghtx1UO82aS6GmL+OO1lLxTzDEnkQGDUc9kKATrfL\r\nHqH3MFb7q3HLZrS+VhZWDQh/ng5HRR1Mt8XVa7Im0wIDAQABoAAwDQYJKoZIhvcN\r\nAQELBQADggEBAK41QA2ACotztfGAP18rG0iSOWdxoFlPGB/Qra9owNCaM60my58K\r\nY8PA8WlY/Nfmke/0ZM1Qed3VGM9SOWmIOSnKSapNMnnMQ5wg1ebetPxm6RU6kMJw\r\nsqZ7n8xENm5/TXEfz7lg1/djU9yfMZAPs0bK8430oFfchx1kPMpM8w3fcOKgo2ii\r\nrc7HwKVbyQTstkuPiNEQzlFM2JtqTuqDDt+Mv1cA2yRrW+LtjuWfuzu1nMjA7F1j\r\n9GWtb/7Jta18oGNfHGZF3nPpRj9FkZJWYwFNq+O95Qsq1I6XWCwTfeFJF8BQYEBK\r\nArduy9FzNG50zzJX0yYI/blzBL/EA27Vvr4=\r\n-----END CERTIFICATE REQUEST-----"
}
~~~

That way, we can now send the CSR to the dojot platform and issue us a
certificate!

#### Obtain the certificate issued by the dojot platform

If all goes well, the dojot platform will issue an x.509 certificate to the CSR
previously informed, the answer given by the *X.509 Identity Management*
component will have the following syntax:

~~~HTTP
HTTP/1.1 201 Created
Content-type: application/json

{
  "certificateFingerprint": <string>,
  "certificatePem": <string>
}
~~~

In practice, we will have something similar to this:

~~~HTTP
HTTP/1.1 201 Created
Content-type: application/json

{
  "certificateFingerprint": "27:98:B3:A2:69:28:2C:C3:00:E2:2C:7D:48:50:94:C4:4A:F7:A2:1C:63:B8:06:7F:69:15:01:F6:EA:09:34:2D",
  "certificatePem": "-----BEGIN CERTIFICATE-----\nMIIG/DCCBOSgAwIBAgIUGeK2VFaRPpGRlBrJjwipf6Eds1YwDQYJKoZIhvcNAQEL\nBQAwejEjMCEGCgmSJomT8ixkAQEME2MtMGZhMzliMzNlNTZlMDY1MTkxGTAXBgNV\nBAMMEFg1MDkgSWRlbnRpdHkgQ0ExGzAZBgNVBAsMEkNlcnRpZmljYXRlIElzc3Vl\ncjEbMBkGA1UECgwSZG9qb3QgSW9UIFBsYXRmb3JtMB4XDTIwMDYwNTIwMDEwMFoX\nDTIxMDYwNTIwMDEwMFowNDEbMBkGA1UECgwSZG9qb3QgSW9UIFBsYXRmb3JtMRUw\nEwYDVQQDDAxhZG1pbjoxMjM0NTYwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEK\nAoIBAQDFu7NweJ79eNKwTkU30Zxd19lsABkNYAXJDqfyrsuT68eeR3nGz0uyQpEE\nSQs0Ob8o62JXvL0DnnAa5MK8yt+p9TiltJEOQXTMbrovFJnDR85BrZFQEbyCKyrz\n9WQTDwCEqv362dnkcwLBd2DObD8OvRg/UzuVoSA7tlsQzaASzpItUJFCuGKzHa0t\nb3Ttrhgz4Q9WPXgOOdKsLg6pouyGSEaXKyIhAcrLUjvnOcSjJbo46+y5iTemW5km\n3gNdbINVluCG3HVQ7zZpLoaYv447WUvFPMMSeRAYNRz2QoBOt8seofcwVvurcctm\ntL5WFlYNCH+eDkdFHUy3xdVrsibTAgMBAAGjggK+MIICujAMBgNVHRMBAf8EAjAA\nMB8GA1UdIwQYMBaAFFjCckdwxGbsWEEv3h/+sy1Cz5RgMIHcBgNVHS4EgdQwgdEw\ngc6ggcuggciGgcVodHRwOi8vMTcyLjE4LjAuMwoxNzIuMTkuMC40OjgwODAvZWpi\nY2EvcHVibGljd2ViL3dlYmRpc3QvY2VydGRpc3Q/Y21kPWRlbHRhY3JsJmlzc3Vl\ncj1VSUQlM0RjLTBmYTM5YjMzZTU2ZTA2NTE5JTJDQ04lM0RYNTA5JTIwSWRlbnRp\ndHklMjBDQSUyQ09VJTNEQ2VydGlmaWNhdGUlMjBJc3N1ZXIlMkNPJTNEZG9qb3Ql\nMjBJb1QlMjBQbGF0Zm9ybTAdBgNVHSUEFjAUBggrBgEFBQcDAgYIKwYBBQUHAwQw\nggFaBgNVHR8EggFRMIIBTTCCAUmggcaggcOGgcBodHRwOi8vMTcyLjE4LjAuMwox\nNzIuMTkuMC40OjgwODAvZWpiY2EvcHVibGljd2ViL3dlYmRpc3QvY2VydGRpc3Q/\nY21kPWNybCZpc3N1ZXI9VUlEJTNEYy0wZmEzOWIzM2U1NmUwNjUxOSUyQ0NOJTNE\nWDUwOSUyMElkZW50aXR5JTIwQ0ElMkNPVSUzRENlcnRpZmljYXRlJTIwSXNzdWVy\nJTJDTyUzRGRvam90JTIwSW9UJTIwUGxhdGZvcm2ifqR8MHoxIzAhBgoJkiaJk/Is\nZAEBDBNjLTBmYTM5YjMzZTU2ZTA2NTE5MRkwFwYDVQQDDBBYNTA5IElkZW50aXR5\nIENBMRswGQYDVQQLDBJDZXJ0aWZpY2F0ZSBJc3N1ZXIxGzAZBgNVBAoMEmRvam90\nIElvVCBQbGF0Zm9ybTAdBgNVHQ4EFgQUuNRRFLrgvwwHe8ShOhMWRISTXZYwDgYD\nVR0PAQH/BAQDAgPoMA0GCSqGSIb3DQEBCwUAA4ICAQCLOCwNFlG6lM3P3+ToiwLA\n33q+ZRnMTXIOzzOKpiaOWfBv55y3nZnHIKuTlNMsP0qiOfDdrdvtQTs5NVmwSfa+\nK0uNH+wFTO8XLgs+Ii7OvAB+UX7+Vo8uxFxPKAgJHOUBFjIZq0y2Le4p+PNtuEQR\ntB3cisT++sUVc6NCgmoRUsEjGxNXnyPVU40GJ9DlNYtlBu4h+FaxaAeeEz3xduU7\neXLjlXpSWSIcRzfGo+6wobENZordAvqz6Hp5o3sHSXhiXkNeO1B2A421bkkUhA7W\nWjE8IknN5QjlquFp5rQZABLTx2Qw7YqiRkLqAXLXM20+XnqKBQ3HjNgklC9CpBZi\nM/KJlEm8muiODENwUBZ6YSo1nsgWAO/YBJ8NAbzT6sUQybsSnCBp9DAOf0amdqYn\na45C0gOWSBXMcxjXXOcVxFdNCfVpFAi9c7Y/bCeYbWVXY8jQ6e3nKwvbvILXORl9\n+Pyqqw6qhToHbcOqdCWqoZoEBRRg06Vkm6r0d6yh/UcE/UHyKlgnRGFZdpFYlSvZ\nTB+4Paeq3wOKxAkjUs2SlFHZYT43+NKmsiSTf1yyKruwh1yMR0hAfRM9GCooOjvr\n5PokzscdSyNwWh/LrkUAvKnB/XgpgqeN+ngK8jVotieV4vLc7ji/i6ME1Czi0g9I\nLPSCByr/Jf5/sLD6GXNh4w==\n-----END CERTIFICATE-----"
}
~~~

In the example above, we have two important observations to make:

 - The first is related to the *fingerprint* of the generated certificate: **A certificate's fingerprint is the unique identifier of the certificate.** Microsoft Internet Explorer calls it *Thumbprint*. Browsers tend to display it as if it were a part of the certificate. It is not a part of the certificate, but it is computed from it.
A fingerprint is the *digest* of the DER-Encoded Certificate Info (which is an ASN.1 type specified as part of the X.509 specification). The Certificate Fingerprint is a digest (hash function) of a certificate in x509 binary format. It can be calculated by different algorithms, such as MD5 or SHA1, **but we chose to work with SHA256**. Note that each pair of hexadecimal characters has been separated by colons. The result of the SHA256 hash function does not include this separation, but we chose to work with it as this is how the fingerprint is presented to the user.
 - The second is related to the certificate, notice that the line break markers are present there, however for the certificate to be used, it is necessary that these markers be replaced by the correct bytes that represent the line break. Note that it is the user's responsibility to save the certificate in [PEM format](https://en.wikipedia.org/wiki/Privacy-Enhanced_Mail), respecting the limit of 64 characters per line.

After extracting the response payload certificate and saving it to disk in a
text file (PEM format), we will have the following result:

    -----BEGIN CERTIFICATE-----
    MIIG/DCCBOSgAwIBAgIUGeK2VFaRPpGRlBrJjwipf6Eds1YwDQYJKoZIhvcNAQEL
    BQAwejEjMCEGCgmSJomT8ixkAQEME2MtMGZhMzliMzNlNTZlMDY1MTkxGTAXBgNV
    BAMMEFg1MDkgSWRlbnRpdHkgQ0ExGzAZBgNVBAsMEkNlcnRpZmljYXRlIElzc3Vl
    cjEbMBkGA1UECgwSZG9qb3QgSW9UIFBsYXRmb3JtMB4XDTIwMDYwNTIwMDEwMFoX
    DTIxMDYwNTIwMDEwMFowNDEbMBkGA1UECgwSZG9qb3QgSW9UIFBsYXRmb3JtMRUw
    EwYDVQQDDAxhZG1pbjoxMjM0NTYwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEK
    AoIBAQDFu7NweJ79eNKwTkU30Zxd19lsABkNYAXJDqfyrsuT68eeR3nGz0uyQpEE
    SQs0Ob8o62JXvL0DnnAa5MK8yt+p9TiltJEOQXTMbrovFJnDR85BrZFQEbyCKyrz
    9WQTDwCEqv362dnkcwLBd2DObD8OvRg/UzuVoSA7tlsQzaASzpItUJFCuGKzHa0t
    b3Ttrhgz4Q9WPXgOOdKsLg6pouyGSEaXKyIhAcrLUjvnOcSjJbo46+y5iTemW5km
    3gNdbINVluCG3HVQ7zZpLoaYv447WUvFPMMSeRAYNRz2QoBOt8seofcwVvurcctm
    tL5WFlYNCH+eDkdFHUy3xdVrsibTAgMBAAGjggK+MIICujAMBgNVHRMBAf8EAjAA
    MB8GA1UdIwQYMBaAFFjCckdwxGbsWEEv3h/+sy1Cz5RgMIHcBgNVHS4EgdQwgdEw
    gc6ggcuggciGgcVodHRwOi8vMTcyLjE4LjAuMwoxNzIuMTkuMC40OjgwODAvZWpi
    Y2EvcHVibGljd2ViL3dlYmRpc3QvY2VydGRpc3Q/Y21kPWRlbHRhY3JsJmlzc3Vl
    cj1VSUQlM0RjLTBmYTM5YjMzZTU2ZTA2NTE5JTJDQ04lM0RYNTA5JTIwSWRlbnRp
    dHklMjBDQSUyQ09VJTNEQ2VydGlmaWNhdGUlMjBJc3N1ZXIlMkNPJTNEZG9qb3Ql
    MjBJb1QlMjBQbGF0Zm9ybTAdBgNVHSUEFjAUBggrBgEFBQcDAgYIKwYBBQUHAwQw
    ggFaBgNVHR8EggFRMIIBTTCCAUmggcaggcOGgcBodHRwOi8vMTcyLjE4LjAuMwox
    NzIuMTkuMC40OjgwODAvZWpiY2EvcHVibGljd2ViL3dlYmRpc3QvY2VydGRpc3Q/
    Y21kPWNybCZpc3N1ZXI9VUlEJTNEYy0wZmEzOWIzM2U1NmUwNjUxOSUyQ0NOJTNE
    WDUwOSUyMElkZW50aXR5JTIwQ0ElMkNPVSUzRENlcnRpZmljYXRlJTIwSXNzdWVy
    JTJDTyUzRGRvam90JTIwSW9UJTIwUGxhdGZvcm2ifqR8MHoxIzAhBgoJkiaJk/Is
    ZAEBDBNjLTBmYTM5YjMzZTU2ZTA2NTE5MRkwFwYDVQQDDBBYNTA5IElkZW50aXR5
    IENBMRswGQYDVQQLDBJDZXJ0aWZpY2F0ZSBJc3N1ZXIxGzAZBgNVBAoMEmRvam90
    IElvVCBQbGF0Zm9ybTAdBgNVHQ4EFgQUuNRRFLrgvwwHe8ShOhMWRISTXZYwDgYD
    VR0PAQH/BAQDAgPoMA0GCSqGSIb3DQEBCwUAA4ICAQCLOCwNFlG6lM3P3+ToiwLA
    33q+ZRnMTXIOzzOKpiaOWfBv55y3nZnHIKuTlNMsP0qiOfDdrdvtQTs5NVmwSfa+
    K0uNH+wFTO8XLgs+Ii7OvAB+UX7+Vo8uxFxPKAgJHOUBFjIZq0y2Le4p+PNtuEQR
    tB3cisT++sUVc6NCgmoRUsEjGxNXnyPVU40GJ9DlNYtlBu4h+FaxaAeeEz3xduU7
    eXLjlXpSWSIcRzfGo+6wobENZordAvqz6Hp5o3sHSXhiXkNeO1B2A421bkkUhA7W
    WjE8IknN5QjlquFp5rQZABLTx2Qw7YqiRkLqAXLXM20+XnqKBQ3HjNgklC9CpBZi
    M/KJlEm8muiODENwUBZ6YSo1nsgWAO/YBJ8NAbzT6sUQybsSnCBp9DAOf0amdqYn
    a45C0gOWSBXMcxjXXOcVxFdNCfVpFAi9c7Y/bCeYbWVXY8jQ6e3nKwvbvILXORl9
    +Pyqqw6qhToHbcOqdCWqoZoEBRRg06Vkm6r0d6yh/UcE/UHyKlgnRGFZdpFYlSvZ
    TB+4Paeq3wOKxAkjUs2SlFHZYT43+NKmsiSTf1yyKruwh1yMR0hAfRM9GCooOjvr
    5PokzscdSyNwWh/LrkUAvKnB/XgpgqeN+ngK8jVotieV4vLc7ji/i6ME1Czi0g9I
    LPSCByr/Jf5/sLD6GXNh4w==
    -----END CERTIFICATE-----

#### How to calculate the certificate _fingerprint_

If you have not saved the certificate _fingerprint_ returned by the API and also
do not know how to calculate it again, you can use the
[OpenSSL](https://www.openssl.org/) tool for this! It is necessary that the
certificate (in PEM format as returned by the API) is saved on disk
(for example, with the name `cert.pem`), then execute the command:

~~~bash
openssl x509 -fingerprint -sha256 -in 'cert.pem'
~~~

<br>
With the certificate in hand, the device already has an identity and is almost
ready to communicate (over TLS) with the dojot platform, but first, it is still
necessary to recover the internal CA certificate of the dojot platform,
which was responsible for sign the device certificate and who the device must
trust for mutual TLS to be established.

This procedure is described below ...

### How to obtain the internal CA certificate of the dojot platform

The syntax for obtaining the internal CA certificate for the dojot platform that
signs the certificates for devices is very simple:

~~~HTTP
GET <base-url>/v1/ca
Authorization: Bearer JWT
~~~

And having the following syntax as the answer:

~~~HTTP
HTTP/1.1 200 Success
Content-type: application/json

{
  "certificateFingerprint": <string>,
  "caPem": <string>
}
~~~

In practice, we will have something similar to this:

~~~HTTP
HTTP/1.1 200 Success
Content-type: application/json

{
  "certificateFingerprint": "B9:7C:88:EE:A4:13:3E:64:99:44:C5:35:61:9D:4E:15:B7:70:A4:FD:78:61:70:22:D2:27:BC:74:15:AA:C5:CC",
  "caPem": "-----BEGIN CERTIFICATE-----\nMIIF6jCCA9KgAwIBAgIUIJGGnpOs/DHPUarctEZt5r7S4IAwDQYJKoZIhvcNAQEL\nBQAwejEjMCEGCgmSJomT8ixkAQEME2MtMGZhMzliMzNlNTZlMDY1MTkxGTAXBgNV\nBAMMEFg1MDkgSWRlbnRpdHkgQ0ExGzAZBgNVBAsMEkNlcnRpZmljYXRlIElzc3Vl\ncjEbMBkGA1UECgwSZG9qb3QgSW9UIFBsYXRmb3JtMCAXDTIwMDYwNTE5MzExMFoY\nDzIwNTAwNTI5MTkzMTEwWjB6MSMwIQYKCZImiZPyLGQBAQwTYy0wZmEzOWIzM2U1\nNmUwNjUxOTEZMBcGA1UEAwwQWDUwOSBJZGVudGl0eSBDQTEbMBkGA1UECwwSQ2Vy\ndGlmaWNhdGUgSXNzdWVyMRswGQYDVQQKDBJkb2pvdCBJb1QgUGxhdGZvcm0wggIi\nMA0GCSqGSIb3DQEBAQUAA4ICDwAwggIKAoICAQDJnP/Oh1gi0Az9vDKOQjzt0CLb\n9O0RJP5TCFdMdGbdVcJ273W8egyTWsu5PnYwyXAYbm613Z2CLHv5rr9mx+9bY27K\nzC8DNcDU05KZN8NuOJYqaBwcfI1x0vMytQE4J64iJzeOeehxEsrxY5wa168MNTDN\n43achUJoxUTVxzdRrUk0Nx2XadfBEHbLOXxffkAE37BahHS/KXywby3EnvJth2kL\nWBifxtD8oDzfCrA9RiwI5Rt5b0ceI9Euprsr4kslxK9uZn7p+/Qx2XX2rHFZqFlz\nKOig61XuusxtbLwbSvWRjPe29hivk18AabWPXqbDuFqPxpW3WkEBUD3Rd/YqEIj1\n2VV0ACcaPkudEx8RmGsTglR3munWIgb1OGckYtd5Lu9+OjkhMSufLCMoBn9sqjPv\nHYmt6paJrlhITw9uF+p8ZBGJ0mb1j+AuRVJEHvsJS8A2bAmtAdTnBsUL+ZflIYB6\nfefAgTtTcdpXppTxgf6Kz156udWgQ7Pvk8Io4Fr4K63ngw2NLdLGsBkR9voGoLJL\nEIllHJodN6jq7z63110LZPdw5Au/55qMibS7389ocbLxvsqxxC7BF+GfnqQ7ooPZ\naE6jsLpHaWRbsT1uDl3rZUu5ijv3HF778N5n5HewBx0DMZE8mtX7oeHmzsDh6HU9\n+94b6Y9NlC8UaKCCkQIDAQABo2YwZDASBgNVHRMBAf8ECDAGAQH/AgEAMB8GA1Ud\nIwQYMBaAFFjCckdwxGbsWEEv3h/+sy1Cz5RgMB0GA1UdDgQWBBRYwnJHcMRm7FhB\nL94f/rMtQs+UYDAOBgNVHQ8BAf8EBAMCAYYwDQYJKoZIhvcNAQELBQADggIBALPy\nHv+6Wm8OxyIwU8kpa/r5HrPcZJIIJn+GA8AFjW/JffUxQN08sITJLUAx2/qnUg5r\nJRPUwWJ64gerJGuY/qavuuArMq+sEXz2Fe5N3p6PtTyyUK2KxRK6SlFFdHmPocRT\nClIRCl+Ae4vpxHjVizzN9tP9RTuxYydf5wejxw2HK9NjMPCCVWiH6qclPeLD9vhw\niHeVISj0fc2tnMbZNDHy/UWFPxIoqV+0nrF/6mCsQE93U0FJENG3oJrwNb3KbR7h\ntWEfUFKWpRjZAKtgdDcWKCIhRZRJ6ToFhq/NsoRjQpXiCRejsVMa+sg8XmaJcG7/\ngLQQlY6x/njOalliJvJmmsV9d27XhgNefWBCM2RWmO/h1Np/vuTOSQ9fEQGa0v8R\nn/fuqfuElastPPbV/UaMHQHvG/PnFYhX8Rbex7koD3Ng+aFtSzJp8xF+8o3qYxeY\nmDrnqFd8JM7GUf2coZt9jHH7n1Ibd2KeMS9FreIETB10jYlcd0+R9oLf7/0yhI5I\nJcGzESkJZD7YATR/ro5X+t22OXjEhiSaoQsXceJURsEYoXHnJx9myy5Tg0Z3Xl7w\nynW39uy69yXippIdCqfD0UyXZiSQ2tXwNVT6cFbGkEbxOjH2zzzmeleFl4EOAkC5\n193nerH/R/ebrNktOaDsmQFKcwnpszpaDT19+lAG\n-----END CERTIFICATE-----"
}
~~~

The answer is very similar to the one we have when requesting a certificate,
just follow the same observations already made in the case of obtaining
certificates for devices.

With the device certificate and the CA certificate that this device trusts
(dojot internal CA) we can now establish secure communication (over TLS)
with the platform.
At this point, the participation of the *X.509 Identity Management* component
ends successfully.

### How to list certificates

There are two ways to obtain certificates issued by the dojot platform.

##### The first and simplest

Just to inform the certificate fingerprint in a GET request according to the syntax:

~~~HTTP
GET <base-url>/v1/certificates/<certificateFingerprint>?fields=<list>
~~~

In practice, we would have something like this:

~~~HTTP
GET <base-url>/v1/certificates/27:98:B3:A2:69:28:2C:C3:00:E2:2C:7D:48:50:94:C4:4A:F7:A2:1C:63:B8:06:7F:69:15:01:F6:EA:09:34:2D
~~~

And as an answer, we will have the following:

~~~HTTP
HTTP/1.1 200 Success
Content-type: application/json

{
  "issuedByDojotPki": true,
  "autoRegistered": false,
  "fingerprint": "27:98:B3:A2:69:28:2C:C3:00:E2:2C:7D:48:50:94:C4:4A:F7:A2:1C:63:B8:06:7F:69:15:01:F6:EA:09:34:2D",
  "pem": "-----BEGIN CERTIFICATE-----\nMIIG/DCCBOSgAwIBAgIUGeK2VFaRPpGRlBrJjwipf6Eds1YwDQYJKoZIhvcNAQEL\nBQAwejEjMCEGCgmSJomT8ixkAQEME2MtMGZhMzliMzNlNTZlMDY1MTkxGTAXBgNV\nBAMMEFg1MDkgSWRlbnRpdHkgQ0ExGzAZBgNVBAsMEkNlcnRpZmljYXRlIElzc3Vl\ncjEbMBkGA1UECgwSZG9qb3QgSW9UIFBsYXRmb3JtMB4XDTIwMDYwNTIwMDEwMFoX\nDTIxMDYwNTIwMDEwMFowNDEbMBkGA1UECgwSZG9qb3QgSW9UIFBsYXRmb3JtMRUw\nEwYDVQQDDAxhZG1pbjoxMjM0NTYwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEK\nAoIBAQDFu7NweJ79eNKwTkU30Zxd19lsABkNYAXJDqfyrsuT68eeR3nGz0uyQpEE\nSQs0Ob8o62JXvL0DnnAa5MK8yt+p9TiltJEOQXTMbrovFJnDR85BrZFQEbyCKyrz\n9WQTDwCEqv362dnkcwLBd2DObD8OvRg/UzuVoSA7tlsQzaASzpItUJFCuGKzHa0t\nb3Ttrhgz4Q9WPXgOOdKsLg6pouyGSEaXKyIhAcrLUjvnOcSjJbo46+y5iTemW5km\n3gNdbINVluCG3HVQ7zZpLoaYv447WUvFPMMSeRAYNRz2QoBOt8seofcwVvurcctm\ntL5WFlYNCH+eDkdFHUy3xdVrsibTAgMBAAGjggK+MIICujAMBgNVHRMBAf8EAjAA\nMB8GA1UdIwQYMBaAFFjCckdwxGbsWEEv3h/+sy1Cz5RgMIHcBgNVHS4EgdQwgdEw\ngc6ggcuggciGgcVodHRwOi8vMTcyLjE4LjAuMwoxNzIuMTkuMC40OjgwODAvZWpi\nY2EvcHVibGljd2ViL3dlYmRpc3QvY2VydGRpc3Q/Y21kPWRlbHRhY3JsJmlzc3Vl\ncj1VSUQlM0RjLTBmYTM5YjMzZTU2ZTA2NTE5JTJDQ04lM0RYNTA5JTIwSWRlbnRp\ndHklMjBDQSUyQ09VJTNEQ2VydGlmaWNhdGUlMjBJc3N1ZXIlMkNPJTNEZG9qb3Ql\nMjBJb1QlMjBQbGF0Zm9ybTAdBgNVHSUEFjAUBggrBgEFBQcDAgYIKwYBBQUHAwQw\nggFaBgNVHR8EggFRMIIBTTCCAUmggcaggcOGgcBodHRwOi8vMTcyLjE4LjAuMwox\nNzIuMTkuMC40OjgwODAvZWpiY2EvcHVibGljd2ViL3dlYmRpc3QvY2VydGRpc3Q/\nY21kPWNybCZpc3N1ZXI9VUlEJTNEYy0wZmEzOWIzM2U1NmUwNjUxOSUyQ0NOJTNE\nWDUwOSUyMElkZW50aXR5JTIwQ0ElMkNPVSUzRENlcnRpZmljYXRlJTIwSXNzdWVy\nJTJDTyUzRGRvam90JTIwSW9UJTIwUGxhdGZvcm2ifqR8MHoxIzAhBgoJkiaJk/Is\nZAEBDBNjLTBmYTM5YjMzZTU2ZTA2NTE5MRkwFwYDVQQDDBBYNTA5IElkZW50aXR5\nIENBMRswGQYDVQQLDBJDZXJ0aWZpY2F0ZSBJc3N1ZXIxGzAZBgNVBAoMEmRvam90\nIElvVCBQbGF0Zm9ybTAdBgNVHQ4EFgQUuNRRFLrgvwwHe8ShOhMWRISTXZYwDgYD\nVR0PAQH/BAQDAgPoMA0GCSqGSIb3DQEBCwUAA4ICAQCLOCwNFlG6lM3P3+ToiwLA\n33q+ZRnMTXIOzzOKpiaOWfBv55y3nZnHIKuTlNMsP0qiOfDdrdvtQTs5NVmwSfa+\nK0uNH+wFTO8XLgs+Ii7OvAB+UX7+Vo8uxFxPKAgJHOUBFjIZq0y2Le4p+PNtuEQR\ntB3cisT++sUVc6NCgmoRUsEjGxNXnyPVU40GJ9DlNYtlBu4h+FaxaAeeEz3xduU7\neXLjlXpSWSIcRzfGo+6wobENZordAvqz6Hp5o3sHSXhiXkNeO1B2A421bkkUhA7W\nWjE8IknN5QjlquFp5rQZABLTx2Qw7YqiRkLqAXLXM20+XnqKBQ3HjNgklC9CpBZi\nM/KJlEm8muiODENwUBZ6YSo1nsgWAO/YBJ8NAbzT6sUQybsSnCBp9DAOf0amdqYn\na45C0gOWSBXMcxjXXOcVxFdNCfVpFAi9c7Y/bCeYbWVXY8jQ6e3nKwvbvILXORl9\n+Pyqqw6qhToHbcOqdCWqoZoEBRRg06Vkm6r0d6yh/UcE/UHyKlgnRGFZdpFYlSvZ\nTB+4Paeq3wOKxAkjUs2SlFHZYT43+NKmsiSTf1yyKruwh1yMR0hAfRM9GCooOjvr\n5PokzscdSyNwWh/LrkUAvKnB/XgpgqeN+ngK8jVotieV4vLc7ji/i6ME1Czi0g9I\nLPSCByr/Jf5/sLD6GXNh4w==\n-----END CERTIFICATE-----",
  "belongsTo": {},
  "tenant": "admin",
  "createdAt": "2020-06-05T20:01:11.286Z"
}
~~~

The optional **fields** parameter in the URL can be used to redefine the content
of the response, being able to list only the desired fields separated by commas:

For example, in the request:

~~~HTTP
GET <base-url>/v1/certificates/27:98:B3:A2:69:28:2C:C3:00:E2:2C:7D:48:50:94:C4:4A:F7:A2:1C:63:B8:06:7F:69:15:01:F6:EA:09:34:2D?fields=pem,createdAt
~~~

We would have the platform's response:

~~~HTTP
HTTP/1.1 200 Success
Content-type: application/json

{
  "pem": "-----BEGIN CERTIFICATE-----\nMIIG/DCCBOSgAwIBAgIUGeK2VFaRPpGRlBrJjwipf6Eds1YwDQYJKoZIhvcNAQEL\nBQAwejEjMCEGCgmSJomT8ixkAQEME2MtMGZhMzliMzNlNTZlMDY1MTkxGTAXBgNV\nBAMMEFg1MDkgSWRlbnRpdHkgQ0ExGzAZBgNVBAsMEkNlcnRpZmljYXRlIElzc3Vl\ncjEbMBkGA1UECgwSZG9qb3QgSW9UIFBsYXRmb3JtMB4XDTIwMDYwNTIwMDEwMFoX\nDTIxMDYwNTIwMDEwMFowNDEbMBkGA1UECgwSZG9qb3QgSW9UIFBsYXRmb3JtMRUw\nEwYDVQQDDAxhZG1pbjoxMjM0NTYwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEK\nAoIBAQDFu7NweJ79eNKwTkU30Zxd19lsABkNYAXJDqfyrsuT68eeR3nGz0uyQpEE\nSQs0Ob8o62JXvL0DnnAa5MK8yt+p9TiltJEOQXTMbrovFJnDR85BrZFQEbyCKyrz\n9WQTDwCEqv362dnkcwLBd2DObD8OvRg/UzuVoSA7tlsQzaASzpItUJFCuGKzHa0t\nb3Ttrhgz4Q9WPXgOOdKsLg6pouyGSEaXKyIhAcrLUjvnOcSjJbo46+y5iTemW5km\n3gNdbINVluCG3HVQ7zZpLoaYv447WUvFPMMSeRAYNRz2QoBOt8seofcwVvurcctm\ntL5WFlYNCH+eDkdFHUy3xdVrsibTAgMBAAGjggK+MIICujAMBgNVHRMBAf8EAjAA\nMB8GA1UdIwQYMBaAFFjCckdwxGbsWEEv3h/+sy1Cz5RgMIHcBgNVHS4EgdQwgdEw\ngc6ggcuggciGgcVodHRwOi8vMTcyLjE4LjAuMwoxNzIuMTkuMC40OjgwODAvZWpi\nY2EvcHVibGljd2ViL3dlYmRpc3QvY2VydGRpc3Q/Y21kPWRlbHRhY3JsJmlzc3Vl\ncj1VSUQlM0RjLTBmYTM5YjMzZTU2ZTA2NTE5JTJDQ04lM0RYNTA5JTIwSWRlbnRp\ndHklMjBDQSUyQ09VJTNEQ2VydGlmaWNhdGUlMjBJc3N1ZXIlMkNPJTNEZG9qb3Ql\nMjBJb1QlMjBQbGF0Zm9ybTAdBgNVHSUEFjAUBggrBgEFBQcDAgYIKwYBBQUHAwQw\nggFaBgNVHR8EggFRMIIBTTCCAUmggcaggcOGgcBodHRwOi8vMTcyLjE4LjAuMwox\nNzIuMTkuMC40OjgwODAvZWpiY2EvcHVibGljd2ViL3dlYmRpc3QvY2VydGRpc3Q/\nY21kPWNybCZpc3N1ZXI9VUlEJTNEYy0wZmEzOWIzM2U1NmUwNjUxOSUyQ0NOJTNE\nWDUwOSUyMElkZW50aXR5JTIwQ0ElMkNPVSUzRENlcnRpZmljYXRlJTIwSXNzdWVy\nJTJDTyUzRGRvam90JTIwSW9UJTIwUGxhdGZvcm2ifqR8MHoxIzAhBgoJkiaJk/Is\nZAEBDBNjLTBmYTM5YjMzZTU2ZTA2NTE5MRkwFwYDVQQDDBBYNTA5IElkZW50aXR5\nIENBMRswGQYDVQQLDBJDZXJ0aWZpY2F0ZSBJc3N1ZXIxGzAZBgNVBAoMEmRvam90\nIElvVCBQbGF0Zm9ybTAdBgNVHQ4EFgQUuNRRFLrgvwwHe8ShOhMWRISTXZYwDgYD\nVR0PAQH/BAQDAgPoMA0GCSqGSIb3DQEBCwUAA4ICAQCLOCwNFlG6lM3P3+ToiwLA\n33q+ZRnMTXIOzzOKpiaOWfBv55y3nZnHIKuTlNMsP0qiOfDdrdvtQTs5NVmwSfa+\nK0uNH+wFTO8XLgs+Ii7OvAB+UX7+Vo8uxFxPKAgJHOUBFjIZq0y2Le4p+PNtuEQR\ntB3cisT++sUVc6NCgmoRUsEjGxNXnyPVU40GJ9DlNYtlBu4h+FaxaAeeEz3xduU7\neXLjlXpSWSIcRzfGo+6wobENZordAvqz6Hp5o3sHSXhiXkNeO1B2A421bkkUhA7W\nWjE8IknN5QjlquFp5rQZABLTx2Qw7YqiRkLqAXLXM20+XnqKBQ3HjNgklC9CpBZi\nM/KJlEm8muiODENwUBZ6YSo1nsgWAO/YBJ8NAbzT6sUQybsSnCBp9DAOf0amdqYn\na45C0gOWSBXMcxjXXOcVxFdNCfVpFAi9c7Y/bCeYbWVXY8jQ6e3nKwvbvILXORl9\n+Pyqqw6qhToHbcOqdCWqoZoEBRRg06Vkm6r0d6yh/UcE/UHyKlgnRGFZdpFYlSvZ\nTB+4Paeq3wOKxAkjUs2SlFHZYT43+NKmsiSTf1yyKruwh1yMR0hAfRM9GCooOjvr\n5PokzscdSyNwWh/LrkUAvKnB/XgpgqeN+ngK8jVotieV4vLc7ji/i6ME1Czi0g9I\nLPSCByr/Jf5/sLD6GXNh4w==\n-----END CERTIFICATE-----",
  "createdAt": "2020-06-05T20:01:11.286Z"
}
~~~

##### The second option

Makes it possible to list several certificates.

The syntax for the listing is given below:

~~~HTTP
GET <base-url>/v1/certificates?page=<number>&limit=<number>&fields=<list>&key1=val1&...&keyn=valn
Authorization: Bearer JWT
~~~

Since the parameters informed in the URL are optional and in case they are not
informed, the list presents the first 25 elements (configurable value).
Below are detailed the parameters that can be informed in the URL:

| Parameter | Description | Type |
|-----------|-------------|------|
| page      | It is a metadata for paginated results. It identifies the position of the first record to be returned; if not set, the value 1 is assumed. | Number (>0) |
| limit     | It is a metadata for paginated results. It defines the maximum number of records (items) to be returned; if not set, the value 25 is assumed. | Number(>0, <=250) |
| fields    | list of record _attributes_ that should be included in the result. If the `fields` parameter is not set, all _attributes_ will be kept in the result. | list of attributes separated by a comma |
| key=val   | Filters the returned records by the given key=val. If more than one key=val pair is set; the returned records must contain all of them.<br>In other words, `key` is the name of an _attribute_ of a record returned by the API. Likewise, `val` contains the filter that should be applied to that _attribute_. For example, if we wanted to filter certificates whose `fingerprint` _started with_ `AF:9B:33`, the parameter would look like this: `fingerprint=^AF:9B:33`. | See [mongo-querystring](https://www.npmjs.com/package/mongo-querystring) for more details |

The response syntax would be as follows:

~~~HTTP
HTTP/1.1 200 Success
Content-type: application/json

{
  "paging": {
    "previous": {
      "number": 1,
      "url": "/x509/v1/certificates?page=1&..."
    },
    "current": {
      "number": 2,
      "url": "/x509/v1/certificates?page=2&..."
    },
    "next": {
      "number": 3,
      "url": "/x509/v1/certificates?page=3&..."
    },
    "totalItems": 60,
    "totalPages": 12,
    "limitPerPage": 5
  },
  "certificates": [
    ...same_as_Get_x.509_Certificate...
  ]
}
~~~

Response Elements

| Key | Description | Type |
|-----|-------------|------|
| certificates | List of x509 certificates according to the query parameters. Each element of the array is the same as specified by Get x509 Certificate. | Array |
| paging | Metadata for controlling paginated results. It contains the URIs for the *previous*, *current* and *next* page. The *previous* and *next* will assume *null* value when there is no page. | Object with keys to navigate the paging. |

A practical example query would be:

~~~HTTP
GET <base-url>/v1/certificates?page=2&limit=3&fields=pem&fingerprint=~:44
~~~

In this case, we are looking at *page* 2, the *limit* of elements per page must
be 3 and we only want the *key "pem"* of each element found, and the filter used
for this is the *fingerprint* containing the string *":44"*.

The platform's response could be something like:

~~~HTTP
HTTP/1.1 200 Success
Content-type: application/json

{
  "paging": {
    "previous": {
      "number": 1,
      "url": "/x509/v1/certificates?page=1&limit=3&fields=pem%2Cfingerprint&fingerprint=~%3A44"
  },
    "current": {
      "number": 2,
      "url": "/x509/v1/certificates?page=2&limit=3&fields=pem%2Cfingerprint&fingerprint=~%3A44"
    },
    "next": {
      "number": 3,
      "url": "/x509/v1/certificates?page=3&limit=3&fields=pem%2Cfingerprint&fingerprint=~%3A44"
    },
    "totalItems": 9,
    "totalPages": 3,
    "limitPerPage": 3
  },
  "certificates": [
    {
      "fingerprint": "77:68:FF:6D:6C:64:88:97:FA:DE:98:6C:E3:7A:3D:AA:1C:52:6F:BF:25:AF:44:B6:02:7F:53:AD:B5:3B:01:C4",
      "pem": "-----BEGIN CERTIFICATE-----\nMIIGRjCCBC6gAwIBAgIURc9riMHOsrnULG9INEC0edGW2RgwDQYJKoZIhvcNAQEL\nBQAwejEjMCEGCgmSJomT8ixkAQEME2MtMGZhMzliMzNlNTZlMDY1MTkxGTAXBgNV\nBAMMEFg1MDkgSWRlbnRpdHkgQ0ExGzAZBgNVBAsMEkNlcnRpZmljYXRlIElzc3Vl\ncjEbMBkGA1UECgwSZG9qb3QgSW9UIFBsYXRmb3JtMB4XDTIwMDYwNjIxMDcwMFoX\nDTIxMDYwNjIxMDcwMFowSTEbMBkGA1UECgwSZG9qb3QgSW9UIFBsYXRmb3JtMSow\nKAYDVQQDDCFhZG1pbjpFQ0MgcHJpbWUyNTZ2MSBUaGlhZ28gVGVzdGUwWTATBgcq\nhkjOPQIBBggqhkjOPQMBBwNCAAScpvJvi3wQGER+b2CQ3/E+Nw+ddTCu2fv5sF/E\nBqVe0tWQu1rWPDmrwmjkIOM33e1fo0SzeNaDrDurPEx7OTAOo4ICvjCCArowDAYD\nVR0TAQH/BAIwADAfBgNVHSMEGDAWgBRYwnJHcMRm7FhBL94f/rMtQs+UYDCB3AYD\nVR0uBIHUMIHRMIHOoIHLoIHIhoHFaHR0cDovLzE3Mi4xOC4wLjMKMTcyLjE5LjAu\nNDo4MDgwL2VqYmNhL3B1YmxpY3dlYi93ZWJkaXN0L2NlcnRkaXN0P2NtZD1kZWx0\nYWNybCZpc3N1ZXI9VUlEJTNEYy0wZmEzOWIzM2U1NmUwNjUxOSUyQ0NOJTNEWDUw\nOSUyMElkZW50aXR5JTIwQ0ElMkNPVSUzRENlcnRpZmljYXRlJTIwSXNzdWVyJTJD\nTyUzRGRvam90JTIwSW9UJTIwUGxhdGZvcm0wHQYDVR0lBBYwFAYIKwYBBQUHAwIG\nCCsGAQUFBwMEMIIBWgYDVR0fBIIBUTCCAU0wggFJoIHGoIHDhoHAaHR0cDovLzE3\nMi4xOC4wLjMKMTcyLjE5LjAuNDo4MDgwL2VqYmNhL3B1YmxpY3dlYi93ZWJkaXN0\nL2NlcnRkaXN0P2NtZD1jcmwmaXNzdWVyPVVJRCUzRGMtMGZhMzliMzNlNTZlMDY1\nMTklMkNDTiUzRFg1MDklMjBJZGVudGl0eSUyMENBJTJDT1UlM0RDZXJ0aWZpY2F0\nZSUyMElzc3VlciUyQ08lM0Rkb2pvdCUyMElvVCUyMFBsYXRmb3Jton6kfDB6MSMw\nIQYKCZImiZPyLGQBAQwTYy0wZmEzOWIzM2U1NmUwNjUxOTEZMBcGA1UEAwwQWDUw\nOSBJZGVudGl0eSBDQTEbMBkGA1UECwwSQ2VydGlmaWNhdGUgSXNzdWVyMRswGQYD\nVQQKDBJkb2pvdCBJb1QgUGxhdGZvcm0wHQYDVR0OBBYEFD1vcBmX/6Of+o7t8eis\nKz8AyLtgMA4GA1UdDwEB/wQEAwID6DANBgkqhkiG9w0BAQsFAAOCAgEAj5PQBQqV\ntZGKiG3RT1v4jE3KwrVadL/L73CHVaBVMEUtIOxE4TOu8eAMPJgiESyu2e8Ga5Na\nu2lRXbdRLv3y4jy8AsHjyylLVYfzLaM+HRZYRLUYX8iHWROdAnfcH9o/INgFnQ4H\nA4C+kn2VEDjhKuK064WNAkLkC/4AYq1TZUWY9CT5jGOLy74Xks1HP4QnZ00iidnj\nXvAJcezWYqZL3ssY3nX/S3m9lsiDLq6qFqs9ZmUzo1NgvRZ/HGWVM/srwEuFujef\ntxoCguC17ctdNYEs09Z7fXSI3uRLfvt4NcO32PFgWZf0fiJxm8auj6pjDMg9Iq/K\nGO7xWyHQmBXNHa2oB/UKw04tgZukBLHm6kKzXN4KPss300dhsigRcWfmcTycfmvI\nb38l45nuycDWEvakSpXdqnYhCWrIaJ5P4OsVDgeQt9zPcyuvkxYlc+v2jrTzoyT7\nD0793JXqb1nHvxxZNJQIulQ3eAfHQCzQsoRBmvJWzbFCBb9/ABBHEtXTbGQuOdSY\ncb3fSdaRSuWQ1TL3lBaC3mDc1pJvDA71KYb9MK4ZDRaj+s8eLcokZaQEussEaIU0\nTgAxiQf4tnnlBkmyTB4DoX3QNNTjZv6U14VaZzcYznrCBSpnWNB0kEwksGGhlpNN\nkUvlhea7lgyv3pmBPVm1z7ZU4B4cmcOcuYA=\n-----END CERTIFICATE-----"
    },
    {
      "fingerprint": "8E:C8:5E:44:7F:BC:E9:77:F4:BB:9F:8B:0D:3B:A2:B0:E4:88:1F:5A:D3:40:3A:31:2A:3D:2F:A2:DF:B1:DB:CB",
      "pem": "-----BEGIN CERTIFICATE-----\nMIIGijCCBHKgAwIBAgIUeDha2NP+nHzIJqitXVTORoM/hQIwDQYJKoZIhvcNAQEL\nBQAwejEjMCEGCgmSJomT8ixkAQEME2MtMGZhMzliMzNlNTZlMDY1MTkxGTAXBgNV\nBAMMEFg1MDkgSWRlbnRpdHkgQ0ExGzAZBgNVBAsMEkNlcnRpZmljYXRlIElzc3Vl\ncjEbMBkGA1UECgwSZG9qb3QgSW9UIFBsYXRmb3JtMB4XDTIwMDYwNjIxMDgwMFoX\nDTIxMDYwNjIxMDgwMFowSjEbMBkGA1UECgwSZG9qb3QgSW9UIFBsYXRmb3JtMSsw\nKQYDVQQDDCJhZG1pbjpUZXN0ZSBkZSBjaGF2ZSBFQ0Mgc2VjcDUyMXIxMIGbMBAG\nByqGSM49AgEGBSuBBAAjA4GGAAQAnBJFrKXkqraaEEpxxvJRx9NUU/NzyPKQkX2f\nnaUQxn6e9Q88BtskqrvkpNeEd6BJP43EuGsrG54wmCmG/HeTAD0BHcvIpcbPgEd7\nUcS4LHtt3DPKuqazet7tD8zlLShs6dCSM3djr+kArxcSjNCJG0iPgEME53Er3jbK\nWbhznLHMUZ2jggK+MIICujAMBgNVHRMBAf8EAjAAMB8GA1UdIwQYMBaAFFjCckdw\nxGbsWEEv3h/+sy1Cz5RgMIHcBgNVHS4EgdQwgdEwgc6ggcuggciGgcVodHRwOi8v\nMTcyLjE4LjAuMwoxNzIuMTkuMC40OjgwODAvZWpiY2EvcHVibGljd2ViL3dlYmRp\nc3QvY2VydGRpc3Q/Y21kPWRlbHRhY3JsJmlzc3Vlcj1VSUQlM0RjLTBmYTM5YjMz\nZTU2ZTA2NTE5JTJDQ04lM0RYNTA5JTIwSWRlbnRpdHklMjBDQSUyQ09VJTNEQ2Vy\ndGlmaWNhdGUlMjBJc3N1ZXIlMkNPJTNEZG9qb3QlMjBJb1QlMjBQbGF0Zm9ybTAd\nBgNVHSUEFjAUBggrBgEFBQcDAgYIKwYBBQUHAwQwggFaBgNVHR8EggFRMIIBTTCC\nAUmggcaggcOGgcBodHRwOi8vMTcyLjE4LjAuMwoxNzIuMTkuMC40OjgwODAvZWpi\nY2EvcHVibGljd2ViL3dlYmRpc3QvY2VydGRpc3Q/Y21kPWNybCZpc3N1ZXI9VUlE\nJTNEYy0wZmEzOWIzM2U1NmUwNjUxOSUyQ0NOJTNEWDUwOSUyMElkZW50aXR5JTIw\nQ0ElMkNPVSUzRENlcnRpZmljYXRlJTIwSXNzdWVyJTJDTyUzRGRvam90JTIwSW9U\nJTIwUGxhdGZvcm2ifqR8MHoxIzAhBgoJkiaJk/IsZAEBDBNjLTBmYTM5YjMzZTU2\nZTA2NTE5MRkwFwYDVQQDDBBYNTA5IElkZW50aXR5IENBMRswGQYDVQQLDBJDZXJ0\naWZpY2F0ZSBJc3N1ZXIxGzAZBgNVBAoMEmRvam90IElvVCBQbGF0Zm9ybTAdBgNV\nHQ4EFgQUKf1jtwgfqvEOnc4mHsYdeVSjSJMwDgYDVR0PAQH/BAQDAgPoMA0GCSqG\nSIb3DQEBCwUAA4ICAQBGIHWpbcnZfvRf5RHnfKnvuey6ksMwu6UwskPwSq1DhWwx\ng1w50xIDxw+QGKWnOrM9AhJWp1mOnO1H0TAxRoXV3l/+pdhHQCtK3t1eEcDM+fyU\nOg8HEotkykYOuu0+Dc8WrhWU96aGwP2zd2zJMhk/BfYleK61Nk9zOvUzVFfzo6ac\n8sdHsxtnlEFsUXhnqFf8d2yYYIuDd1Et6nlLqq8p6pOnSb4BuqtWi53oXOc+x6MT\n5OsVgYJQ3IZrpgosCnqxq3myFse+qrY5jPtvMAjhqk9hvB/9R160WmZjV8PlOpeX\nqSnJQgCxD7S7EF+F8PGlKZNlpxVDhpyKlxpBK8OPYPRqxzjPZyzC1OuYt78VLHh4\np0zteX5QokfII8CY3I0PfOl1icatFvt4f9GEa/l3D0k9NM4uTka8fDEBQbhsycyJ\nBTDEcgr01zFqEUsg3tg7NN7s2/ky/Legsb5b4WF2lnG7cYhf7IXPG0gECjc1q4cb\nHLC69uDfBthIPE6mAxaL/JEoDF8RsNiYTQsP5+6MxWoFuD6NL4nL6K8WE5rttfeV\n34f+UPkC6mAEbYQLS2zOLK+92wwRWdWwxIQu+yP1d3aNaj9XHxjKUkpSY9Lj0mdU\ns/TJfx7Verz6j0/E4ZucR8fiYK+k1b1rvE3NAHP20wCaQw33Y1Vtyn/ru9CYZw==\n-----END CERTIFICATE-----"
    },
    {
      "fingerprint": "B9:6B:39:F3:4A:1C:44:FB:16:6F:49:E2:0B:19:67:8B:27:45:D4:6B:76:B0:76:1D:3B:8B:1D:90:75:FB:BB:85",
      "pem": "-----BEGIN CERTIFICATE-----\nMIIGSDCCBDCgAwIBAgIUJJfmrkQJHdNWIrXCsR2dp67jj2swDQYJKoZIhvcNAQEL\nBQAwejEjMCEGCgmSJomT8ixkAQEME2MtMGZhMzliMzNlNTZlMDY1MTkxGTAXBgNV\nBAMMEFg1MDkgSWRlbnRpdHkgQ0ExGzAZBgNVBAsMEkNlcnRpZmljYXRlIElzc3Vl\ncjEbMBkGA1UECgwSZG9qb3QgSW9UIFBsYXRmb3JtMB4XDTIwMDYwNjIxMTMwMFoX\nDTIxMDYwNjIxMTMwMFowSzEbMBkGA1UECgwSZG9qb3QgSW9UIFBsYXRmb3JtMSww\nKgYDVQQDDCNhZG1pbjpUZXN0ZSBkZSBjaGF2ZSBFQ0MgcHJpbWUyNTZ2MTBZMBMG\nByqGSM49AgEGCCqGSM49AwEHA0IABOX06A6uSaC7iS0kXo33kTSXoT2Q/80vRdXk\nNjNcyYXqZSmwHn33XKS0k8Ij2wNel5vIQe+NPs3B5opP1MtYvkujggK+MIICujAM\nBgNVHRMBAf8EAjAAMB8GA1UdIwQYMBaAFFjCckdwxGbsWEEv3h/+sy1Cz5RgMIHc\nBgNVHS4EgdQwgdEwgc6ggcuggciGgcVodHRwOi8vMTcyLjE4LjAuMwoxNzIuMTku\nMC40OjgwODAvZWpiY2EvcHVibGljd2ViL3dlYmRpc3QvY2VydGRpc3Q/Y21kPWRl\nbHRhY3JsJmlzc3Vlcj1VSUQlM0RjLTBmYTM5YjMzZTU2ZTA2NTE5JTJDQ04lM0RY\nNTA5JTIwSWRlbnRpdHklMjBDQSUyQ09VJTNEQ2VydGlmaWNhdGUlMjBJc3N1ZXIl\nMkNPJTNEZG9qb3QlMjBJb1QlMjBQbGF0Zm9ybTAdBgNVHSUEFjAUBggrBgEFBQcD\nAgYIKwYBBQUHAwQwggFaBgNVHR8EggFRMIIBTTCCAUmggcaggcOGgcBodHRwOi8v\nMTcyLjE4LjAuMwoxNzIuMTkuMC40OjgwODAvZWpiY2EvcHVibGljd2ViL3dlYmRp\nc3QvY2VydGRpc3Q/Y21kPWNybCZpc3N1ZXI9VUlEJTNEYy0wZmEzOWIzM2U1NmUw\nNjUxOSUyQ0NOJTNEWDUwOSUyMElkZW50aXR5JTIwQ0ElMkNPVSUzRENlcnRpZmlj\nYXRlJTIwSXNzdWVyJTJDTyUzRGRvam90JTIwSW9UJTIwUGxhdGZvcm2ifqR8MHox\nIzAhBgoJkiaJk/IsZAEBDBNjLTBmYTM5YjMzZTU2ZTA2NTE5MRkwFwYDVQQDDBBY\nNTA5IElkZW50aXR5IENBMRswGQYDVQQLDBJDZXJ0aWZpY2F0ZSBJc3N1ZXIxGzAZ\nBgNVBAoMEmRvam90IElvVCBQbGF0Zm9ybTAdBgNVHQ4EFgQU6HusqT6OTWtfZEzP\nKp/Vwg9vOeAwDgYDVR0PAQH/BAQDAgPoMA0GCSqGSIb3DQEBCwUAA4ICAQC84YXy\nXr55KV2pWLxwbcos/PIIzz7ttTyRqBfsWtPgN23UsCD87+5SrhzuEynMb2tbhL78\n/DrYiaXtjy+QFIcUxrwuN6J8w2opotXkQop2mV09I/0cg/lY8RGyyGL6KJAef5gh\n8ujuje5oB6iFkOB2PiEyLYif9Vj+GluJik505raQK79KLULEnZALLjWQczUR76KU\nh/zTGS9B5aq2zmoYyH3lUQ73bt4Xt8LfzcjXie3cz93zJ9gNVTQspD1MpQGEdv+G\nIjdlVuNaIjwQclrT7PmKy4VIPzXOHaCxQlrbkySJCoqQbDjSYyPVVxxXcH7kWvJ9\nSypDu0wZEuL7D0TPh93cdcNIIV0QC8C72u4G54jZVGajwGGnLw8MGxEgOTAdPSC4\nkruRc15JoKfwussIC4LcXov1Q/xxfq22nfAKKgY4R6HG32XsLY4EAQwYkwLw2X97\nU2mPkhDRW5ikxHLLRKGwMb5menc4aX4zzRBGkiTxTu/RrUvHsoeLPvm7n2swyeup\nvAhZ7/Fh8Yv98vWLIBeO8p5Rg3mOAUKaAmz6yzbxSAKnV4XJi5QBjxnPJTHwOhbw\ndFZhNmcqeavnrbcx3gtmWSlg5/0VoYk0j4LByMpqvSmu/ygA60/uRJ8jzNBOHI2d\naCgQ6zhUKqzvBpr2WUhlnT8UYUG/jWmTLgy/ZQ==\n-----END CERTIFICATE-----"
    }
  ]
}
~~~

### How to revoke a certificate

Revoking certificates on the dojot platform has the same meaning as revoking
certificates on a [PKI](https://en.wikipedia.org/wiki/Public_key_infrastructure).
The certificate revocation process consists of the user informing that a
certificate (still within the validity period) should no longer be accepted by
the platform. This is usually done when the certificate's private key has been
compromised.

**The platform does not revoke the certificate instantly**, what happens is that
from time to time (configurable value), the platform generates
[CRLs](https://en.wikipedia.org/wiki/Certificate_revocation_list)
(Certificate revocation list) and makes them available to the *Device Broker*.
The *Broker*, in turn, has the time to update its CRL. Keep in mind that for the
platform as a whole, this process is very expensive to execute with each
revocation request, therefore, remember that a certificate is still valid as
soon as the user requests its revocation, this takes a certain time
(configurable) until the platform updates its CRLs.

The syntax for revoking a certificate issued by the dojot platform is as follows:

~~~HTTP
DELETE <base-url>/v1/certificates/<certificateFingerprint>
Authorization: Bearer JWT
~~~

The `certificateFingerprint` is the same as that informed when the certificate
was issued. Of course, it is also possible to calculate it using the SHA256 hash
function, note that the dojot platform works with the colon character as a
separator for each byte (represented by two values in the hexadecimal base).

In practice, we would have something like this:

~~~HTTP
DELETE <base-url>/v1/certificates/27:98:B3:A2:69:28:2C:C3:00:E2:2C:7D:48:50:94:C4:4A:F7:A2:1C:63:B8:06:7F:69:15:01:F6:EA:09:34:2D
~~~

The answer would be a simple confirmation:

~~~HTTP
HTTP/1.1 204 No Content
~~~


### How to register a trusted CA certificate

Certificates from trusted CAs can be registered through endpoint
`/api/v1/trusted-cas` of the *X.509 Identity Management* component, but as in
practice the component is behind an API Gateway, the URI becomes
`http://{host}:{port}/x509/v1/trusted-cas`.
Therefore, you must send the trusted CA according to the syntax:

~~~HTTP
POST <base-url>/v1/trusted-cas
Authorization: Bearer JWT
Content-type: application/json

{
  "caPem": <string>
}
~~~

The trusted CA certificate is nothing more than a text file (PEM format), it has
the following format:

    -----BEGIN CERTIFICATE-----
    MIICNTCCAZigAwIBAgIUGis7n29EcOsEoCxCm980onT5sR4wCgYIKoZIzj0EAwIw
    LTErMCkGA1UEAwwiUm9vdCBDQSBkdW1teSAoZG9qb3QgSW9UIFBsYXRmb3JtKTAe
    Fw0yMDA5MzAxMzI2MDRaFw0yNTA5MjkxMzI2MDRaMC0xKzApBgNVBAMMIlJvb3Qg
    Q0EgZHVtbXkgKGRvam90IElvVCBQbGF0Zm9ybSkwgZswEAYHKoZIzj0CAQYFK4EE
    ACMDgYYABAFTleScH0EakSco7iPtMN76N3h9PvR7l1UDzYLiDkgYch3W4FwGUDCS
    9yBtaKEiMEv8hGHMHzf0Jsy03hse6DjSagCYWpMqXlYSUZ5muKD7IPC4l+T9KLbB
    mWptY8NQTMiFPZDs2OcLPaaGKJwN42EKBjEyC9dS+WaBFRYFajDvg9rUrKNTMFEw
    HQYDVR0OBBYEFNEcTxzYBiTx3QvnLxnsdHdJySJ2MB8GA1UdIwQYMBaAFNEcTxzY
    BiTx3QvnLxnsdHdJySJ2MA8GA1UdEwEB/wQFMAMBAf8wCgYIKoZIzj0EAwIDgYoA
    MIGGAkFhyI5TpL5voJWQPhjo1AqKzwGcwA3KcSv+Gmz8dbIBt5G+tDmZi213x8CB
    lxNWsrb3Q7+M1emWwrPc/bfXcSHZZwJBZMX4XC6wJTRHkrjNV28evQb+mYbpJl8M
    uK6pmr2SWatOSndPkhgOY5VPGEsnrVpT0OGrUkoe9khvdaDX2yFlAXY=
    -----END CERTIFICATE-----

Note that this format consists of a header and a footer, the CA certificate is
encoded in *Base64* in the middle. Also note that each line has a maximum of
64 characters.
As we have to send this file in a JSON payload, it is necessary to represent the
line breaks with the `\r\n` (or just `\n`) markers.
That done, we will have the following payload:

~~~javascript
{
  "caPem": "-----BEGIN CERTIFICATE-----\nMIICNTCCAZigAwIBAgIUGis7n29EcOsEoCxCm980onT5sR4wCgYIKoZIzj0EAwIw\nLTErMCkGA1UEAwwiUm9vdCBDQSBkdW1teSAoZG9qb3QgSW9UIFBsYXRmb3JtKTAe\nFw0yMDA5MzAxMzI2MDRaFw0yNTA5MjkxMzI2MDRaMC0xKzApBgNVBAMMIlJvb3Qg\nQ0EgZHVtbXkgKGRvam90IElvVCBQbGF0Zm9ybSkwgZswEAYHKoZIzj0CAQYFK4EE\nACMDgYYABAFTleScH0EakSco7iPtMN76N3h9PvR7l1UDzYLiDkgYch3W4FwGUDCS\n9yBtaKEiMEv8hGHMHzf0Jsy03hse6DjSagCYWpMqXlYSUZ5muKD7IPC4l+T9KLbB\nmWptY8NQTMiFPZDs2OcLPaaGKJwN42EKBjEyC9dS+WaBFRYFajDvg9rUrKNTMFEw\nHQYDVR0OBBYEFNEcTxzYBiTx3QvnLxnsdHdJySJ2MB8GA1UdIwQYMBaAFNEcTxzY\nBiTx3QvnLxnsdHdJySJ2MA8GA1UdEwEB/wQFMAMBAf8wCgYIKoZIzj0EAwIDgYoA\nMIGGAkFhyI5TpL5voJWQPhjo1AqKzwGcwA3KcSv+Gmz8dbIBt5G+tDmZi213x8CB\nlxNWsrb3Q7+M1emWwrPc/bfXcSHZZwJBZMX4XC6wJTRHkrjNV28evQb+mYbpJl8M\nuK6pmr2SWatOSndPkhgOY5VPGEsnrVpT0OGrUkoe9khvdaDX2yFlAXY=\n-----END CERTIFICATE-----"
}
~~~

That way, we can send the trusted CA certificate to the dojot platform!

In response, the platform returns the _fingerprint_ of the registered CA
certificate, as follows:

~~~HTTP
HTTP/1.1 201 Created
Content-type: application/json

{
  "caFingerprint": "24:59:61:1B:0C:C4:14:43:D9:B1:15:A0:D3:46:E3:58:23:53:60:66:21:82:A3:E6:BA:72:04:C0:3F:B7:9B:FA"
}
~~~

__NOTE THAT__ some checks are made on the certificate before it is registered,
they are:

- Checks the minimum number of days remaining until the certificate expires;
- Checks whether the certificate is in fact a root CA certificate;
- Checks whether the certificate has a _Common Name_ different from that of the platform CA;
- Checks the limit of CA certificates allowed by tenant;
- Checks whether the certificate has been previously registered;

If all checks pass, the certificate is registered in the database.

### How to remove a trusted CA certificate

The syntax for remove a trusted CA certificate is as follows:

~~~HTTP
DELETE <base-url>/v1/trusted-cas/<caCertificateFingerprint>
Authorization: Bearer JWT
~~~

The `caCertificateFingerprint` is the same as that returned when the CA
certificate was registered. Of course, it is also possible to calculate it using
the SHA256 hash function, note that the dojot platform works with the colon
character as a separator for each byte (represented by two values in the
hexadecimal base).

In practice, we would have something like this:

~~~HTTP
DELETE <base-url>/v1/trusted-cas/24:59:61:1B:0C:C4:14:43:D9:B1:15:A0:D3:46:E3:58:23:53:60:66:21:82:A3:E6:BA:72:04:C0:3F:B7:9B:FA
~~~

The answer would be a simple confirmation:

~~~HTTP
HTTP/1.1 204 No Content
~~~

__NOTE THAT__ if there are certificates registered on the platform that in turn
were issued by the CA to be removed, if these certificates have not been
self-registered, it will not be possible to remove the certificate from the CA
until its dependents are all removed.
For certificates issued by the CA that have been self-registered, these will be
removed automatically when that CA is removed as well.


### How to list trusted CA certificates

The mechanism for listing certificates from trusted CAs is the same as that used
in certificates issued by the dojot platform, only bearing in mind that the
endpoint is as follows:

~~~HTTP
GET <base-url>/v1/trusted-cas/<caCertificateFingerprint>
Authorization: Bearer JWT
~~~

See the [listing certificates](#list-issued-certificates) for more details of
what can be done to filter certificates from trusted CAs.

### How to enable/disable self-registration of certificates issued by a trusted CA

The option to enable self-registration of certificates issued by a trusted CA
can be provided at the time of registering the CA certificate, or by updating
the CA certificate record.
To update the value of this flag, just make the following request:

~~~HTTP
PATCH <base-url>/v1/trusted-cas/<caCertificateFingerprint>
Authorization: Bearer JWT
Content-type: application/json

{
  "allowAutoRegistration": true
}
~~~

The `caCertificateFingerprint` is the same as that returned when the CA
certificate was registered. Of course, it is also possible to calculate it using
the SHA256 hash function, note that the dojot platform works with the colon
character as a separator for each byte (represented by two values in the
hexadecimal base).

In practice, we would have something like this:

~~~HTTP
PATCH <base-url>/v1/trusted-cas/24:59:61:1B:0C:C4:14:43:D9:B1:15:A0:D3:46:E3:58:23:53:60:66:21:82:A3:E6:BA:72:04:C0:3F:B7:9B:FA
Authorization: Bearer JWT
Content-type: application/json

{
  "allowAutoRegistration": true
}
~~~

The answer would be a simple confirmation:

~~~HTTP
HTTP/1.1 204 No Content
~~~

__NOTE THAT__ enabling the self-registration of certificates issued by a trusted
CA implies that any certificate issued by such a CA will be accepted as an
identifier of some device, this may not be desired if the CA is used to issue
certificates to devices of _several_ tenants. In this case, there would be a
security breach, as a device from a certain tenant could pass itself off as a
device from another tenant.
This option is ideal when the tenant has _its own CA_, so the other tenants will
never have certificates issued by that CA.


### How to register an external certificate (issued by a trusted CA)

The registration of an external certificate must be done to identify a device
that already has a factory certificate. It is not necessary to identify the
device immediately, if you just want to register the certificate and associate
it with a device later, it is enough that the CA that issued such certificate
is previously registered on the platform.
It is possible to register the CA and the external certificate at once,
but this is an option that must be enabled in the service.

The requirement to register an external certificate is as follows:

~~~HTTP
POST <base-url>/v1/certificates
Authorization: Bearer JWT
Content-type: application/json

{
  "certificateChain": <string>,
  "caFingerprint": <string>,
  "belongsTo": {
    "device": <string>
  }
}
~~~

In attribute `certificateChain`, the _chain of trust_ of the certificate to be
registered is informed, the chain starts with the certificate of the device and
goes to the end of the chain of trust that ends in the CA certificate trusted
by the platform.

There is the option to inform the certificate of the trusted CA in the chain,
or just its `caFingerprint` to decrease the size of the payload, since the
certificate of the trusted CA must have been previously registered on the
platform. However, it is still necessary that the certificates of the
intermediate CAs are in the chain of trust (if any).
If the option to register the trusted CA together with the external certificate
is enabled, it will be necessary for the root CA certificate to be in the
chain of trust.

The `belongsTo` attribute identifies the `device` with which the external
certificate will be associated. This attribute is not mandatory, and the client
can inform this attribute later when changing the certificate registration.

The _chain of trust_ of the certificate is nothing more than a text file
(PEM format), it has the following format:

    -----BEGIN CERTIFICATE-----
    MIIHJTCCBg2gAwIBAgISA/c80WOrBS1B0YKU1WnbOIwuMA0GCSqGSIb3DQEBCwUA
    MEoxCzAJBgNVBAYTAlVTMRYwFAYDVQQKEw1MZXQncyBFbmNyeXB0MSMwIQYDVQQD
    ExpMZXQncyBFbmNyeXB0IEF1dGhvcml0eSBYMzAeFw0yMDEwMDUxMzAyNDRaFw0y
    MTAxMDMxMzAyNDRaMB4xHDAaBgNVBAMMEyouc3RhY2tleGNoYW5nZS5jb20wggEi
    MA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQDgvEf4788HVB81wIAnFbY556Qb
    7BOB5IhjozLwLS9OsOAn2Dmr+P/456nysCXQAFw/Y98R+INfjTScScZa+WfKM9tk
    TSLrrHuPyFQ0IEwpy59+cdnPoJQWrAu6Y0RGRv27yOOVRyeAqge2pArDiYqrc0sE
    HSrBSS1wsq/nnzcaSZroL9uBqGi8hhe5GJUYk2F5EiexsYxv9jx8uLQ7vpBmk3Et
    JbOlP00unQZH5Wd6swTntOhFUHSE2g3Bj3Wi/Mjhq6spTQmvjazN6+ZT6l+UEFSI
    8PdlS9cH99DlPyVxiZfezobk9CGAfkhWhFRoecXKIeMGY49jUmicuZJfa5A7AgMB
    AAGjggQvMIIEKzAOBgNVHQ8BAf8EBAMCBaAwHQYDVR0lBBYwFAYIKwYBBQUHAwEG
    CCsGAQUFBwMCMAwGA1UdEwEB/wQCMAAwHQYDVR0OBBYEFK+7kfNW1XVWKaiJnPL+
    LA+dQ6qqMB8GA1UdIwQYMBaAFKhKamMEfd265tE5t6ZFZe/zqOyhMG8GCCsGAQUF
    BwEBBGMwYTAuBggrBgEFBQcwAYYiaHR0cDovL29jc3AuaW50LXgzLmxldHNlbmNy
    eXB0Lm9yZzAvBggrBgEFBQcwAoYjaHR0cDovL2NlcnQuaW50LXgzLmxldHNlbmNy
    eXB0Lm9yZy8wggHkBgNVHREEggHbMIIB14IPKi5hc2t1YnVudHUuY29tghIqLmJs
    b2dvdmVyZmxvdy5jb22CEioubWF0aG92ZXJmbG93Lm5ldIIYKi5tZXRhLnN0YWNr
    ZXhjaGFuZ2UuY29tghgqLm1ldGEuc3RhY2tvdmVyZmxvdy5jb22CESouc2VydmVy
    ZmF1bHQuY29tgg0qLnNzdGF0aWMubmV0ghMqLnN0YWNrZXhjaGFuZ2UuY29tghMq
    LnN0YWNrb3ZlcmZsb3cuY29tghUqLnN0YWNrb3ZlcmZsb3cuZW1haWyCDyouc3Vw
    ZXJ1c2VyLmNvbYINYXNrdWJ1bnR1LmNvbYIQYmxvZ292ZXJmbG93LmNvbYIQbWF0
    aG92ZXJmbG93Lm5ldIIUb3BlbmlkLnN0YWNrYXV0aC5jb22CD3NlcnZlcmZhdWx0
    LmNvbYILc3N0YXRpYy5uZXSCDXN0YWNrYXBwcy5jb22CDXN0YWNrYXV0aC5jb22C
    EXN0YWNrZXhjaGFuZ2UuY29tghJzdGFja292ZXJmbG93LmJsb2eCEXN0YWNrb3Zl
    cmZsb3cuY29tghNzdGFja292ZXJmbG93LmVtYWlsghFzdGFja3NuaXBwZXRzLm5l
    dIINc3VwZXJ1c2VyLmNvbTBMBgNVHSAERTBDMAgGBmeBDAECATA3BgsrBgEEAYLf
    EwEBATAoMCYGCCsGAQUFBwIBFhpodHRwOi8vY3BzLmxldHNlbmNyeXB0Lm9yZzCC
    AQMGCisGAQQB1nkCBAIEgfQEgfEA7wB1AJQgvB6O1Y1siHMfgosiLA3R2k1ebE+U
    PWHbTi9YTaLCAAABdPkSXP4AAAQDAEYwRAIgVay70Cu9d46NEOmUt3XUu7bXIqkS
    h+DQXw0Rdy5qIQ0CIH4GmNouXeCovRlx/T4B9Hh//+VvA1tBakgiq+6WEPR8AHYA
    fT7y+I//iFVoJMLAyp5SiXkrxQ54CX8uapdomX4i8NcAAAF0+RJdVgAABAMARzBF
    AiEAs4iZyvg1zC2zaFCs9CNuiGhkuD3cdmcuPCx1qi7rZqcCIAQIaxcyd5wkVWNj
    1CeXrUriThrMyOElkNXaN34j3WqUMA0GCSqGSIb3DQEBCwUAA4IBAQA5BQYZcDBu
    h1NnUYspMTFcuDjYSmZDlD9MBTSaA4alsHN2l+jsz/cLgPNZWdOhn1NPb6OU3x4J
    AOz/4waQvqQ0VYhjBplLMiH3HPXHIiaHJw+p+Hdz0gi3gMcvuoz7ifu+9GemmdGV
    wdpeGuZP4NQXJCnuNhwjrqFQHuoimKvm2M555fJB+ij+p3K2KhbQnq2BKnn2EqIR
    OX9Euhv1TVpUz+rSSJJ89tIUAqzpHSS6CJt3Z3Ljgtyy1u0J1+UNlJ69JNEZIhsG
    fcfc6rV6/wF3uRRBdJck9qyMCejg7NESyxTGnj+QcgbzEpMbGdzZ0PCyvaJWccl7
    qysRzGiJF1WI
    -----END CERTIFICATE-----
    -----BEGIN CERTIFICATE-----
    MIIEkjCCA3qgAwIBAgIQCgFBQgAAAVOFc2oLheynCDANBgkqhkiG9w0BAQsFADA/
    MSQwIgYDVQQKExtEaWdpdGFsIFNpZ25hdHVyZSBUcnVzdCBDby4xFzAVBgNVBAMT
    DkRTVCBSb290IENBIFgzMB4XDTE2MDMxNzE2NDA0NloXDTIxMDMxNzE2NDA0Nlow
    SjELMAkGA1UEBhMCVVMxFjAUBgNVBAoTDUxldCdzIEVuY3J5cHQxIzAhBgNVBAMT
    GkxldCdzIEVuY3J5cHQgQXV0aG9yaXR5IFgzMIIBIjANBgkqhkiG9w0BAQEFAAOC
    AQ8AMIIBCgKCAQEAnNMM8FrlLke3cl03g7NoYzDq1zUmGSXhvb418XCSL7e4S0EF
    q6meNQhY7LEqxGiHC6PjdeTm86dicbp5gWAf15Gan/PQeGdxyGkOlZHP/uaZ6WA8
    SMx+yk13EiSdRxta67nsHjcAHJyse6cF6s5K671B5TaYucv9bTyWaN8jKkKQDIZ0
    Z8h/pZq4UmEUEz9l6YKHy9v6Dlb2honzhT+Xhq+w3Brvaw2VFn3EK6BlspkENnWA
    a6xK8xuQSXgvopZPKiAlKQTGdMDQMc2PMTiVFrqoM7hD8bEfwzB/onkxEz0tNvjj
    /PIzark5McWvxI0NHWQWM6r6hCm21AvA2H3DkwIDAQABo4IBfTCCAXkwEgYDVR0T
    AQH/BAgwBgEB/wIBADAOBgNVHQ8BAf8EBAMCAYYwfwYIKwYBBQUHAQEEczBxMDIG
    CCsGAQUFBzABhiZodHRwOi8vaXNyZy50cnVzdGlkLm9jc3AuaWRlbnRydXN0LmNv
    bTA7BggrBgEFBQcwAoYvaHR0cDovL2FwcHMuaWRlbnRydXN0LmNvbS9yb290cy9k
    c3Ryb290Y2F4My5wN2MwHwYDVR0jBBgwFoAUxKexpHsscfrb4UuQdf/EFWCFiRAw
    VAYDVR0gBE0wSzAIBgZngQwBAgEwPwYLKwYBBAGC3xMBAQEwMDAuBggrBgEFBQcC
    ARYiaHR0cDovL2Nwcy5yb290LXgxLmxldHNlbmNyeXB0Lm9yZzA8BgNVHR8ENTAz
    MDGgL6AthitodHRwOi8vY3JsLmlkZW50cnVzdC5jb20vRFNUUk9PVENBWDNDUkwu
    Y3JsMB0GA1UdDgQWBBSoSmpjBH3duubRObemRWXv86jsoTANBgkqhkiG9w0BAQsF
    AAOCAQEA3TPXEfNjWDjdGBX7CVW+dla5cEilaUcne8IkCJLxWh9KEik3JHRRHGJo
    uM2VcGfl96S8TihRzZvoroed6ti6WqEBmtzw3Wodatg+VyOeph4EYpr/1wXKtx8/
    wApIvJSwtmVi4MFU5aMqrSDE6ea73Mj2tcMyo5jMd6jmeWUHK8so/joWUoHOUgwu
    X4Po1QYz+3dszkDqMp4fklxBwXRsW10KXzPMTZ+sOPAveyxindmjkW8lGy+QsRlG
    PfZ+G6Z6h7mjem0Y+iWlkYcV4PIWL1iwBi8saCbGS5jN2p8M+X+Q7UNKEkROb3N6
    KOqkqm57TH2H3eDJAkSnh6/DNFu0Qg==
    -----END CERTIFICATE-----
    -----BEGIN CERTIFICATE-----
    MIIDSjCCAjKgAwIBAgIQRK+wgNajJ7qJMDmGLvhAazANBgkqhkiG9w0BAQUFADA/
    MSQwIgYDVQQKExtEaWdpdGFsIFNpZ25hdHVyZSBUcnVzdCBDby4xFzAVBgNVBAMT
    DkRTVCBSb290IENBIFgzMB4XDTAwMDkzMDIxMTIxOVoXDTIxMDkzMDE0MDExNVow
    PzEkMCIGA1UEChMbRGlnaXRhbCBTaWduYXR1cmUgVHJ1c3QgQ28uMRcwFQYDVQQD
    Ew5EU1QgUm9vdCBDQSBYMzCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEB
    AN+v6ZdQCINXtMxiZfaQguzH0yxrMMpb7NnDfcdAwRgUi+DoM3ZJKuM/IUmTrE4O
    rz5Iy2Xu/NMhD2XSKtkyj4zl93ewEnu1lcCJo6m67XMuegwGMoOifooUMM0RoOEq
    OLl5CjH9UL2AZd+3UWODyOKIYepLYYHsUmu5ouJLGiifSKOeDNoJjj4XLh7dIN9b
    xiqKqy69cK3FCxolkHRyxXtqqzTWMIn/5WgTe1QLyNau7Fqckh49ZLOMxt+/yUFw
    7BZy1SbsOFU5Q9D8/RhcQPGX69Wam40dutolucbY38EVAjqr2m7xPi71XAicPNaD
    aeQQmxkqtilX4+U9m5/wAl0CAwEAAaNCMEAwDwYDVR0TAQH/BAUwAwEB/zAOBgNV
    HQ8BAf8EBAMCAQYwHQYDVR0OBBYEFMSnsaR7LHH62+FLkHX/xBVghYkQMA0GCSqG
    SIb3DQEBBQUAA4IBAQCjGiybFwBcqR7uKGY3Or+Dxz9LwwmglSBd49lZRNI+DT69
    ikugdB/OEIKcdBodfpga3csTS7MgROSR6cz8faXbauX+5v3gTt23ADq1cEmv8uXr
    AvHRAosZy5Q6XkjEGB5YGV8eAlrwDPGxrancWYaLbumR9YbK+rlmM6pZW87ipxZz
    R8srzJmwN0jP41ZL9c8PDHIyh8bwRLtTcm1D9SZImlJnt1ir/md2cXjbDaJWFBM5
    JDGFoqgCWjBH4d1QB7wCCZAA62RjYJsWvIjJEubSfZGL+T0yjWW06XyxV3bqxbYo
    Ob8VZRzI9neWagqNdwvYkQsEjgfbKbYK7p2CNTUQ
    -----END CERTIFICATE-----

Note that this format consists of a header and a footer, the CA certificate is
encoded in *Base64* in the middle. Also note that each line has a maximum of
64 characters.
As we have to send this file in a JSON payload, it is necessary to represent the
line breaks with the `\r\n` (or just `\n`) markers.
That done, we will have the following payload:

~~~javascript
{
  "certificateChain": "-----BEGIN CERTIFICATE-----\nMIIHJTCCBg2gAwIBAgISA/c80WOrBS1B0YKU1WnbOIwuMA0GCSqGSIb3DQEBCwUA\nMEoxCzAJBgNVBAYTAlVTMRYwFAYDVQQKEw1MZXQncyBFbmNyeXB0MSMwIQYDVQQD\nExpMZXQncyBFbmNyeXB0IEF1dGhvcml0eSBYMzAeFw0yMDEwMDUxMzAyNDRaFw0y\nMTAxMDMxMzAyNDRaMB4xHDAaBgNVBAMMEyouc3RhY2tleGNoYW5nZS5jb20wggEi\nMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQDgvEf4788HVB81wIAnFbY556Qb\n7BOB5IhjozLwLS9OsOAn2Dmr+P/456nysCXQAFw/Y98R+INfjTScScZa+WfKM9tk\nTSLrrHuPyFQ0IEwpy59+cdnPoJQWrAu6Y0RGRv27yOOVRyeAqge2pArDiYqrc0sE\nHSrBSS1wsq/nnzcaSZroL9uBqGi8hhe5GJUYk2F5EiexsYxv9jx8uLQ7vpBmk3Et\nJbOlP00unQZH5Wd6swTntOhFUHSE2g3Bj3Wi/Mjhq6spTQmvjazN6+ZT6l+UEFSI\n8PdlS9cH99DlPyVxiZfezobk9CGAfkhWhFRoecXKIeMGY49jUmicuZJfa5A7AgMB\nAAGjggQvMIIEKzAOBgNVHQ8BAf8EBAMCBaAwHQYDVR0lBBYwFAYIKwYBBQUHAwEG\nCCsGAQUFBwMCMAwGA1UdEwEB/wQCMAAwHQYDVR0OBBYEFK+7kfNW1XVWKaiJnPL+\nLA+dQ6qqMB8GA1UdIwQYMBaAFKhKamMEfd265tE5t6ZFZe/zqOyhMG8GCCsGAQUF\nBwEBBGMwYTAuBggrBgEFBQcwAYYiaHR0cDovL29jc3AuaW50LXgzLmxldHNlbmNy\neXB0Lm9yZzAvBggrBgEFBQcwAoYjaHR0cDovL2NlcnQuaW50LXgzLmxldHNlbmNy\neXB0Lm9yZy8wggHkBgNVHREEggHbMIIB14IPKi5hc2t1YnVudHUuY29tghIqLmJs\nb2dvdmVyZmxvdy5jb22CEioubWF0aG92ZXJmbG93Lm5ldIIYKi5tZXRhLnN0YWNr\nZXhjaGFuZ2UuY29tghgqLm1ldGEuc3RhY2tvdmVyZmxvdy5jb22CESouc2VydmVy\nZmF1bHQuY29tgg0qLnNzdGF0aWMubmV0ghMqLnN0YWNrZXhjaGFuZ2UuY29tghMq\nLnN0YWNrb3ZlcmZsb3cuY29tghUqLnN0YWNrb3ZlcmZsb3cuZW1haWyCDyouc3Vw\nZXJ1c2VyLmNvbYINYXNrdWJ1bnR1LmNvbYIQYmxvZ292ZXJmbG93LmNvbYIQbWF0\naG92ZXJmbG93Lm5ldIIUb3BlbmlkLnN0YWNrYXV0aC5jb22CD3NlcnZlcmZhdWx0\nLmNvbYILc3N0YXRpYy5uZXSCDXN0YWNrYXBwcy5jb22CDXN0YWNrYXV0aC5jb22C\nEXN0YWNrZXhjaGFuZ2UuY29tghJzdGFja292ZXJmbG93LmJsb2eCEXN0YWNrb3Zl\ncmZsb3cuY29tghNzdGFja292ZXJmbG93LmVtYWlsghFzdGFja3NuaXBwZXRzLm5l\ndIINc3VwZXJ1c2VyLmNvbTBMBgNVHSAERTBDMAgGBmeBDAECATA3BgsrBgEEAYLf\nEwEBATAoMCYGCCsGAQUFBwIBFhpodHRwOi8vY3BzLmxldHNlbmNyeXB0Lm9yZzCC\nAQMGCisGAQQB1nkCBAIEgfQEgfEA7wB1AJQgvB6O1Y1siHMfgosiLA3R2k1ebE+U\nPWHbTi9YTaLCAAABdPkSXP4AAAQDAEYwRAIgVay70Cu9d46NEOmUt3XUu7bXIqkS\nh+DQXw0Rdy5qIQ0CIH4GmNouXeCovRlx/T4B9Hh//+VvA1tBakgiq+6WEPR8AHYA\nfT7y+I//iFVoJMLAyp5SiXkrxQ54CX8uapdomX4i8NcAAAF0+RJdVgAABAMARzBF\nAiEAs4iZyvg1zC2zaFCs9CNuiGhkuD3cdmcuPCx1qi7rZqcCIAQIaxcyd5wkVWNj\n1CeXrUriThrMyOElkNXaN34j3WqUMA0GCSqGSIb3DQEBCwUAA4IBAQA5BQYZcDBu\nh1NnUYspMTFcuDjYSmZDlD9MBTSaA4alsHN2l+jsz/cLgPNZWdOhn1NPb6OU3x4J\nAOz/4waQvqQ0VYhjBplLMiH3HPXHIiaHJw+p+Hdz0gi3gMcvuoz7ifu+9GemmdGV\nwdpeGuZP4NQXJCnuNhwjrqFQHuoimKvm2M555fJB+ij+p3K2KhbQnq2BKnn2EqIR\nOX9Euhv1TVpUz+rSSJJ89tIUAqzpHSS6CJt3Z3Ljgtyy1u0J1+UNlJ69JNEZIhsG\nfcfc6rV6/wF3uRRBdJck9qyMCejg7NESyxTGnj+QcgbzEpMbGdzZ0PCyvaJWccl7\nqysRzGiJF1WI\n-----END CERTIFICATE-----\n-----BEGIN CERTIFICATE-----\nMIIEkjCCA3qgAwIBAgIQCgFBQgAAAVOFc2oLheynCDANBgkqhkiG9w0BAQsFADA/\nMSQwIgYDVQQKExtEaWdpdGFsIFNpZ25hdHVyZSBUcnVzdCBDby4xFzAVBgNVBAMT\nDkRTVCBSb290IENBIFgzMB4XDTE2MDMxNzE2NDA0NloXDTIxMDMxNzE2NDA0Nlow\nSjELMAkGA1UEBhMCVVMxFjAUBgNVBAoTDUxldCdzIEVuY3J5cHQxIzAhBgNVBAMT\nGkxldCdzIEVuY3J5cHQgQXV0aG9yaXR5IFgzMIIBIjANBgkqhkiG9w0BAQEFAAOC\nAQ8AMIIBCgKCAQEAnNMM8FrlLke3cl03g7NoYzDq1zUmGSXhvb418XCSL7e4S0EF\nq6meNQhY7LEqxGiHC6PjdeTm86dicbp5gWAf15Gan/PQeGdxyGkOlZHP/uaZ6WA8\nSMx+yk13EiSdRxta67nsHjcAHJyse6cF6s5K671B5TaYucv9bTyWaN8jKkKQDIZ0\nZ8h/pZq4UmEUEz9l6YKHy9v6Dlb2honzhT+Xhq+w3Brvaw2VFn3EK6BlspkENnWA\na6xK8xuQSXgvopZPKiAlKQTGdMDQMc2PMTiVFrqoM7hD8bEfwzB/onkxEz0tNvjj\n/PIzark5McWvxI0NHWQWM6r6hCm21AvA2H3DkwIDAQABo4IBfTCCAXkwEgYDVR0T\nAQH/BAgwBgEB/wIBADAOBgNVHQ8BAf8EBAMCAYYwfwYIKwYBBQUHAQEEczBxMDIG\nCCsGAQUFBzABhiZodHRwOi8vaXNyZy50cnVzdGlkLm9jc3AuaWRlbnRydXN0LmNv\nbTA7BggrBgEFBQcwAoYvaHR0cDovL2FwcHMuaWRlbnRydXN0LmNvbS9yb290cy9k\nc3Ryb290Y2F4My5wN2MwHwYDVR0jBBgwFoAUxKexpHsscfrb4UuQdf/EFWCFiRAw\nVAYDVR0gBE0wSzAIBgZngQwBAgEwPwYLKwYBBAGC3xMBAQEwMDAuBggrBgEFBQcC\nARYiaHR0cDovL2Nwcy5yb290LXgxLmxldHNlbmNyeXB0Lm9yZzA8BgNVHR8ENTAz\nMDGgL6AthitodHRwOi8vY3JsLmlkZW50cnVzdC5jb20vRFNUUk9PVENBWDNDUkwu\nY3JsMB0GA1UdDgQWBBSoSmpjBH3duubRObemRWXv86jsoTANBgkqhkiG9w0BAQsF\nAAOCAQEA3TPXEfNjWDjdGBX7CVW+dla5cEilaUcne8IkCJLxWh9KEik3JHRRHGJo\nuM2VcGfl96S8TihRzZvoroed6ti6WqEBmtzw3Wodatg+VyOeph4EYpr/1wXKtx8/\nwApIvJSwtmVi4MFU5aMqrSDE6ea73Mj2tcMyo5jMd6jmeWUHK8so/joWUoHOUgwu\nX4Po1QYz+3dszkDqMp4fklxBwXRsW10KXzPMTZ+sOPAveyxindmjkW8lGy+QsRlG\nPfZ+G6Z6h7mjem0Y+iWlkYcV4PIWL1iwBi8saCbGS5jN2p8M+X+Q7UNKEkROb3N6\nKOqkqm57TH2H3eDJAkSnh6/DNFu0Qg==\n-----END CERTIFICATE-----\n-----BEGIN CERTIFICATE-----\nMIIDSjCCAjKgAwIBAgIQRK+wgNajJ7qJMDmGLvhAazANBgkqhkiG9w0BAQUFADA/\nMSQwIgYDVQQKExtEaWdpdGFsIFNpZ25hdHVyZSBUcnVzdCBDby4xFzAVBgNVBAMT\nDkRTVCBSb290IENBIFgzMB4XDTAwMDkzMDIxMTIxOVoXDTIxMDkzMDE0MDExNVow\nPzEkMCIGA1UEChMbRGlnaXRhbCBTaWduYXR1cmUgVHJ1c3QgQ28uMRcwFQYDVQQD\nEw5EU1QgUm9vdCBDQSBYMzCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEB\nAN+v6ZdQCINXtMxiZfaQguzH0yxrMMpb7NnDfcdAwRgUi+DoM3ZJKuM/IUmTrE4O\nrz5Iy2Xu/NMhD2XSKtkyj4zl93ewEnu1lcCJo6m67XMuegwGMoOifooUMM0RoOEq\nOLl5CjH9UL2AZd+3UWODyOKIYepLYYHsUmu5ouJLGiifSKOeDNoJjj4XLh7dIN9b\nxiqKqy69cK3FCxolkHRyxXtqqzTWMIn/5WgTe1QLyNau7Fqckh49ZLOMxt+/yUFw\n7BZy1SbsOFU5Q9D8/RhcQPGX69Wam40dutolucbY38EVAjqr2m7xPi71XAicPNaD\naeQQmxkqtilX4+U9m5/wAl0CAwEAAaNCMEAwDwYDVR0TAQH/BAUwAwEB/zAOBgNV\nHQ8BAf8EBAMCAQYwHQYDVR0OBBYEFMSnsaR7LHH62+FLkHX/xBVghYkQMA0GCSqG\nSIb3DQEBBQUAA4IBAQCjGiybFwBcqR7uKGY3Or+Dxz9LwwmglSBd49lZRNI+DT69\nikugdB/OEIKcdBodfpga3csTS7MgROSR6cz8faXbauX+5v3gTt23ADq1cEmv8uXr\nAvHRAosZy5Q6XkjEGB5YGV8eAlrwDPGxrancWYaLbumR9YbK+rlmM6pZW87ipxZz\nR8srzJmwN0jP41ZL9c8PDHIyh8bwRLtTcm1D9SZImlJnt1ir/md2cXjbDaJWFBM5\nJDGFoqgCWjBH4d1QB7wCCZAA62RjYJsWvIjJEubSfZGL+T0yjWW06XyxV3bqxbYo\nOb8VZRzI9neWagqNdwvYkQsEjgfbKbYK7p2CNTUQ\n-----END CERTIFICATE-----"
}
~~~

That way, we can send the external certificate to the dojot platform!

In response, the platform returns the _fingerprint_ of the registered
certificate, as follows:

~~~HTTP
HTTP/1.1 201 Created
Content-type: application/json

{
  "certificateFingerprint": "E5:81:5A:DF:11:A9:0C:CC:51:8F:6A:99:D2:6C:67:16:29:D6:68:E1:EA:C2:C0:A7:E7:9B:84:09:AF:9C:29:14"
}
~~~

__NOTE THAT__ some checks are made on the certificate before it is registered,
they are:

- Checks the minimum number of days remaining until the certificate expires;
- Checks whether the certificate is not a CA certificate;
- Checks whether the certificate has been previously registered;

If all checks pass, the certificate is registered in the database.


### How to remove an external certificate

To remove an external certificate, the process is the same used to revoke a
certificate issued by the platform itself, just do as follows:

~~~HTTP
DELETE <base-url>/v1/certificates/<certificateFingerprint>
Authorization: Bearer JWT
~~~

The `certificateFingerprint` is the same as that returned when the certificate
was registered. Of course, it is also possible to calculate it using
the SHA256 hash function, note that the dojot platform works with the colon
character as a separator for each byte (represented by two values in the
hexadecimal base).

In practice, we would have something like this:

~~~HTTP
DELETE <base-url>/v1/certificates/27:98:B3:A2:69:28:2C:C3:00:E2:2C:7D:48:50:94:C4:4A:F7:A2:1C:63:B8:06:7F:69:15:01:F6:EA:09:34:2D
~~~

The answer would be a simple confirmation:

~~~HTTP
HTTP/1.1 204 No Content
~~~


### How to associate a device with a certificate

First of all, it is worth mentioning that the fastest way to associate a device
with a certificate is in the same operation in which the certificate is
_generated_ (from the sending of a CSR) or _registered as trusted_ (from the
sending of a PEM certificate external). In this case, it is enough to inform the
attribute `belongsTo.device` inside the payload in the request body.

However, there is the case where the certificate is registered even before a
device is registered on the platform. therefore, the association is made in a
step subsequent to the registration of the certificate.

To associate a device represented on the platform with a certificate, simply
enter the device identifier for the certificate endpoint, as follows:

~~~HTTP
PATCH <base-url>/v1/certificates/<certificateFingerprint>
Authorization: Bearer JWT
Content-type: application/json

{
  "belongsTo" : {
    "device" : <string>
  }
}
~~~

The `certificateFingerprint` is the same as that returned when the certificate
was registered. Of course, it is also possible to calculate it using
the SHA256 hash function, note that the dojot platform works with the colon
character as a separator for each byte (represented by two values in the
hexadecimal base).

In practice, we would have something like this:

~~~HTTP
PATCH <base-url>/v1/certificates/E5:81:5A:DF:11:A9:0C:CC:51:8F:6A:99:D2:6C:67:16:29:D6:68:E1:EA:C2:C0:A7:E7:9B:84:09:AF:9C:29:14
Authorization: Bearer JWT
Content-type: application/json

{
  "belongsTo" : {
    "device" : "123456"
  }
}
~~~

The answer would be a simple confirmation:

~~~HTTP
HTTP/1.1 204 No Content
~~~

This approach serves both to associate a device with a certificate and to change
the device associated with the certificate.

__Note that__ we can also call this operation _Certificate Ownership Creation_
or _Certificate Ownership Change_ in the case where the certificate already had
a previous owner.

### How to unassociate a device with a certificate

To undo a device's association with a certificate, simply pass the `null` value
as the device's identifier, as follows:

~~~HTTP
PATCH <base-url>/v1/certificates/<certificateFingerprint>
Authorization: Bearer JWT
Content-type: application/json

{
  "belongsTo" : {
    "device" : null
  }
}
~~~

An alternative to this mode is to use the `DELETE` HTTP method on the
`<base-url>/v1/certificates/<certificateFingerprint>/belongsto` endpoint,
thus, it is not necessary to inform a body for the request, as follows:

~~~HTTP
DELETE <base-url>/v1/certificates/<certificateFingerprint>/belongsto
Authorization: Bearer JWT
~~~

__Caution:__ Pay close attention to the endpoint `.../belongsto` suffix so as
not to remove the _certificate record_ instead of the _association_!

Both approaches will have the same result, that is, the device will no longer be
associated with the certificate. The difference is that using the `DELETE`
method will remove the `belongsTo.device` sub attribute from the certificate
record, while the `PATCH` method will keep the sub attribute with a `null`
value. In practice, the _service_ behaves in the same way for both approaches.

__Note that__ we can also call this operation _Certificate Ownership Removal_.

## Running the service

### Configurations

Before running the **x509-identity-mgmt** service within your environment, make
sure you configure the environment variables to match your needs.

You can select the configuration file via the `X509IDMGMT_USER_CONFIG_FILE`
variable. Its default value is `production.conf`. Check the
[config directory](./js/config) for the user configurations that are
available by default.

For more information about the usage of the configuration files and environment
variables, check the __ConfigManager__ module in our
[Microservice SDK](https://github.com/dojot/dojot-microservice-sdk-js).
You can also check the
[ConfigManager environment variables documentation](https://github.com/dojot/dojot-microservice-sdk-js/blob/master/lib/configManager/README.md#environment-variables)
for more details.

In short, all the parameters in the next sections are mapped to environment
variables that begin with `X509IDMGMT_`. You can either use environment
variables or configuration files to change their values.
You can also create new parameters via environment variables by following the
fore mentioned convention.

#### HTTP Server settings

| Key | type | Default Value | Valid Values | Environment variable | Purpose |
|-----|------|---------------|--------------|----------------------|---------|
| server.port | integer | `3000` | from 0 to 65535 | X509IDMGMT_SERVER_PORT | Port on which the application listens for http requests. |
| server.cert | string | | | X509IDMGMT_SERVER_CERT | Path to the certificate-file.pem |
| server.key  | string | | | X509IDMGMT_SERVER_KEY  | Path to the private.key |
| server.ca   | string | | | X509IDMGMT_SERVER_CA   | Path to the ca-certificate-file.pem |
| server.healthcheck.port | integer | `9000` | from 0 to 65535 | X509IDMGMT_SERVER_HEALTHCHECK_PORT | port for service Health Check. |
| server.shutdown.delay   | integer | `10000` | positive values | X509IDMGMT_SERVER_SHUTDOWN_DELAY | Delay (in milliseconds) until the shutdown routine starts. |
| server.shutdown.gracefultimeoutms | integer | `60000` | positive values | X509IDMGMT_SERVER_SHUTDOWN_GRACEFULTIMEOUTMS | Number of milliseconds to wait until the process finishes normally after receiving a shutdown signal, after which the process is forced to end. |
| server.shutdown.handlertimeoutms | integer | `5000` | positive values | X509IDMGMT_SERVER_SHUTDOWN_HANDLERTIMEOUTMS | Number of milliseconds waiting for execution of the shutdown handlers, if this value is exceeded, the process is forced to end. |


#### Express Framework settings

| Key | type | Default Value | Valid Values | Environment variable | Purpose |
|-----|------|---------------|--------------|----------------------|---------|
| framework.trustproxy | boolean | `true` | `true` or `false` | X509IDMGMT_FRAMEWORK_TRUSTPROXY | By enabling the trust proxy feature, the client IP address will be updated in the correct places with the forwarded IP. If the service is running behind a reverse proxy, it is necessary to configure the reverse proxy to forward the client's real IP address (http://expressjs.com/en/guide/behind-proxies.html). |
| framework.logformat | string | `:id "HTTP/:http-version :method :url" ":status :res[content-length]bytes :total-time[0]ms" ":remote-addr :referrer :user-agent"` | https://expressjs.com/en/resources/middleware/morgan.html | X509IDMGMT_FRAMEWORK_LOGFORMAT | Log format of requests arriving to the service. |
| framework.bodyparser.limit | string | `100kb` | http://expressjs.com/en/resources/middleware/body-parser.html | X509IDMGMT_FRAMEWORK_BODYPARSER_LIMIT | Controls the maximum request body size. |
| framework.paginate.limit | integer | `25` | positive values | X509IDMGMT_FRAMEWORK_PAGINATE_LIMIT | A number to limit results returned per page. |
| framework.paginate.maxlimit | integer | `250` | positive values | X509IDMGMT_FRAMEWORK_PAGINATE_MAXLIMIT | A number to restrict the number of results returned to per page. Through this, users will not be able to override this limit. |


#### EJBCA Integration settings

| Key | type | Default Value | Valid Values | Environment variable | Purpose |
|-----|------|---------------|--------------|----------------------|---------|
| ejbca.healthcheck.url | string | `http://127.0.0.1:8080/ejbca/publicweb/healthcheck/ejbcahealth` | | X509IDMGMT_EJBCA_HEALTHCHECK_URL | EJBCA Server Health Check URL. |
| ejbca.healthcheck.delayms | integer | `10000` | positive values | X509IDMGMT_EJBCA_HEALTHCHECK_DELAYMS | A number in milliseconds that the service is checking if the EJBCA is healthy. |
| ejbca.wsdl | string | `https://127.0.0.1:8443/ejbca/ejbcaws/ejbcaws?wsdl` | | X509IDMGMT_EJBCA_WSDL | URL for the WSDL that describes the operations available on the EJBCA via SOAP. |
| ejbca.pkcs12 | string | `/opt/tls/ejbcaclient.p12` | | X509IDMGMT_EJBCA_PKCS12 | The [PKCS#12](https://en.wikipedia.org/wiki/PKCS_12) file generated so that the service can establish communication with the EJBCA server. |
| ejbca.pkcs12secret | string | `/opt/tls/ejbcaclient.secret` | | X509IDMGMT_EJBCA_PKCS12SECRET | Secret to access the .p12 file. |
| ejbca.trustedca | string | `/opt/tls/ejbcaclient-trustedca.pem` | | X509IDMGMT_EJBCA_TRUSTEDCA | EJBCA root CA certificate. |
| ejbca.forcecrlrenew | boolean | `false` | `true` or `false` | X509IDMGMT_EJBCA_FORCECRLRENEW | Forces CRL renewal every time it is requested. |
| ejbca.rootca | string | `X509 Identity CA` | | X509IDMGMT_EJBCA_ROOTCA | Name of the CA that signs certificates for IoT devices. |


#### Certificate Control Settings

| Key | type | Default Value | Valid Values | Environment variable | Purpose |
|-----|------|---------------|--------------|----------------------|---------|
| certificate.subject.allowedattrs | string[] | `["CN"]` | | X509IDMGMT_CERTIFICATE_SUBJECT_ALLOWEDATTRS | SubjectDN fields allowed to be entered by the user in the CSR when requesting a certificate for an IoT device. The list of fields must be separated by commas. |
| certificate.subject.allowedattrsconstraints | string[] | `["CN=^[0-9A-Za-z ]{1,255}$"]` | | X509IDMGMT_CERTIFICATE_SUBJECT_ALLOWEDATTRSCONSTRAINTS | Constraints ([RegExp](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/RegExp)) on the values of the allowed SubjectDN fields. <br>The values must represent a map (key=value), the separator of map entries must be separated by commas. <br>Exemple: `["C=^[A-Z]{2,3}\$","ST=^[A-Za-z ]{1,50}\$"]`<br>This means that the value for the *Country* field is limited to the regular expression: `^[A-Z]{2,3}$`<br>Also, the *State* field is limited to the regular expression: `^[A-Za-z ]{1,50}$` |
| certificate.subject.mandatoryattrs | string[] | `["CN"]` | | X509IDMGMT_CERTIFICATE_SUBJECT_MANDATORYATTRS | Mandatory fields that must be present in the CSR |
| certificate.subject.constantattrs.o | string | `dojot IoT Platform` | | X509IDMGMT_CERTIFICATE_SUBJECT_CONSTANTATTRS_O | In particular, the SubjectDN *Organization* field has a constant value and cannot be changed by a CSR. |
| certificate.validity | integer | `365` | positive values | X509IDMGMT_CERTIFICATE_VALIDITY | certificate validity (in days). |
| certificate.check.publickey | boolean | `true` | `true` or `false` | X509IDMGMT_CERTIFICATE_CHECK_PUBLICKEY | Flag indicating whether the CSR public key validation should be performed or not. |
| certificate.check.subjectdn | boolean | `false` | `true` or `false` | X509IDMGMT_CERTIFICATE_CHECK_SUBJECTDN | Flag indicating whether the CSR SubjectDN field validation should be performed or not. |
| certificate.belongsto.application | string[] | `["kafka-consumer", "vernemq", "v2k-bridge", "k2v-bridge"]` | String array, where each element must pass through the regular expression: `^[0-9a-zA-Z-]{1,64}$` | X509IDMGMT_CERTIFICATE_BELONGSTO_APPLICATION | list of application names to which the service can issue a certificate. |
| certificate.external.minimumvaliditydays | integer | `1` | positive values | X509IDMGMT_CERTIFICATE_EXTERNAL_MINIMUMVALIDITYDAYS | Minimum validity (in days) that the external certificate must still have before it can be registered. |
| certificate.external.ca.minimumvaliditydays | integer | `1` | positive values | X509IDMGMT_CERTIFICATE_EXTERNAL_CA_MINIMUMVALIDITYDAYS | Minimum validity (in days) that the external CA certificate must still have before it can be registered. |
| certificate.external.ca.limit | integer | `-1` | | X509IDMGMT_CERTIFICATE_EXTERNAL_CA_LIMIT | Limit of certificates from external CAs that can be registered by tenant. Any negative value indicates that there will be no limit on the number of CA certificates that can be registered by tenants. |
| certificate.external.ca.autoregistration | boolean | `false` | `true` or `false` | X509IDMGMT_CERTIFICATE_EXTERNAL_CA_AUTOREGISTRATION | Whether or not to enable automatic registration of the root CA when an external device certificate is about to be registered as a trusted certificate. |


#### MongoDBClient/mongoose Settings

| Key | type | Default Value | Valid Values | Environment variable | Purpose |
|-----|------|---------------|--------------|----------------------|---------|
| mongo.conn.uri | string | `mongodb://127.0.0.1:27017/x509-identity-mgmt` | | X509IDMGMT_MONGO_CONN_URI | MongoDB [Connection String](https://docs.mongodb.com/manual/reference/connection-string/). |
| mongo.conn.options.user | string | undefined | | X509IDMGMT_MONGO_CONN_OPTIONS_USER | The username for auth (e.g. 'root') |
| mongo.conn.options.pass | string | undefined | | X509IDMGMT_MONGO_CONN_OPTIONS_PASS | The password for auth (e.g. 'pass') |
| mongo.conn.options.authsource | string | undefined | | X509IDMGMT_MONGO_CONN_OPTIONS_AUTHSOURCE | Define the database to authenticate against (e.g. 'admin') |
| mongo.conn.options.autoindex | boolean | `true` | `true` or `false` | X509IDMGMT_MONGO_CONN_OPTIONS_AUTOINDEX | By default, mongoose will automatically build indexes defined in your schema when it connects. This is great for development, but not ideal for large production deployments, because index builds can cause performance degradation. If you set autoIndex to false, mongoose will not automatically build indexes for any model |
| mongo.conn.options.poolsize | integer | `100` | positive values | X509IDMGMT_MONGO_CONN_OPTIONS_POOLSIZE | The maximum number of sockets the MongoDB driver will keep open |
| mongo.conn.options.serverselectiontimeoutms | integer | `30000` | positive values | X509IDMGMT_MONGO_CONN_OPTIONS_SERVERSELECTIONTIMEOUTMS | How long to wait for a connection to be established before timing out |
| mongo.conn.options.heartbeatfrequencyms | integer | `10000` | positive values | X509IDMGMT_MONGO_CONN_OPTIONS_HEARTBEATFREQUENCYMS | controls when the driver checks the state of the MongoDB deployment. Specify the interval (in milliseconds) between checks. |
| mongo.conn.options.sockettimeoutms | integer | `360000` | positive values | X509IDMGMT_MONGO_CONN_OPTIONS_SOCKETTIMEOUTMS | How long a send or receive on a socket can take before timing out. |
| mongo.conn.options.family | integer | `0` | `4`, `6` or `0` | X509IDMGMT_MONGO_CONN_OPTIONS_FAMILY | Version of IP stack. Can be 4, 6 or 0 (default). if 0, will attempt to connect with IPv6, and will fall back to IPv4 on failure. |
| mongo.query.maxtimems | integer | `30000` | positive values | X509IDMGMT_MONGO_QUERY_MAXTIMEMS | Sets the maxTimeMS option. This will tell the MongoDB server to abort if the query or write op has been running for more than ms milliseconds. |


#### Logger Settings

| Key | type | Default Value | Valid Values | Environment variable | Purpose |
|-----|------|---------------|--------------|----------------------|---------|
| logger.verbose | boolean | `false` | `true` or `false` | X509IDMGMT_LOGGER_VERBOSE | In verbose mode, file and line of where the logging method has been called are added as metadata to the logging message. |
| logger.console.level | string | `info` | `error`, `warn`, `info` or `debug` | X509IDMGMT_LOGGER_CONSOLE_LEVEL | logging level. |
| logger.file.enable | boolean | `false` | `true` or `false` | X509IDMGMT_LOGGER_FILE_ENABLE | Flag that enables logging to file. |
| logger.file.level | string | `info` | `error`, `warn`, `info` or `debug` | X509IDMGMT_LOGGER_FILE_LEVEL | logging level. |
| logger.file.dir | string | `./temp/log/` | | X509IDMGMT_LOGGER_FILE_DIR | Directory where the log files will be saved. |
| logger.file.name | string | `dojot.x509-identity-mgmt-%DATE%.log` | | X509IDMGMT_LOGGER_FILE_NAME | Log file name pattern. |
| logger.file.max | string | `7d` | | X509IDMGMT_LOGGER_FILE_MAX | Maximum number of logs to keep. If not set, no logs will be removed. This can be a number of files or number of days. If using days, add 'd' as the suffix. |
| logger.file.size | string | `10m` | | X509IDMGMT_LOGGER_FILE_SIZE | Maximum size of the file after which it will rotate. This can be a number of bytes, or units of kb, mb, and gb. If using the units, add 'k', 'm', or 'g' as the suffix. The units need to directly follow the number. |


#### Kafka Integration Settings

| Key | type | Default Value | Valid Values | Environment variable | Purpose |
|-----|------|---------------|--------------|----------------------|---------|
| kafka.topic.acks | integer | `-1` | `all`, `-1`, `0`, `1` | X509IDMGMT_KAFKA_TOPIC_ACKS | The number of acknowledgments the producer requires the leader to have received before considering a request complete. See more [here](https://kafka.apache.org/documentation/#producerconfigs_acks). |
| kafka.topic.auto.offset.reset | string | `latest` | `latest`, `earliest`, `none` | X509IDMGMT_KAFKA_TOPIC_AUTO_OFFSET_RESET | What to do when there is no initial offset in Kafka or if the current offset does not exist any more on the server. See more [here](https://kafka.apache.org/documentation/#consumerconfigs_auto.offset.reset). |
| kafka.consumer.client.id | string | `x509-identity-mgmt` | - | X509IDMGMT_KAFKA_CONSUMER_CLIENT_ID | An id string to pass to the server when making requests. See more [here](https://kafka.apache.org/documentation/#consumerconfigs_client.id). |
| kafka.consumer.group.id | string | `x509-identity-mgmt-group` | - | X509IDMGMT_KAFKA_CONSUMER_GROUP_ID | A unique string that identifies the consumer group this consumer belongs to. See more [here](https://kafka.apache.org/documentation/#consumerconfigs_group.id). |
| kafka.consumer.max.in.flight.req.per.conn | integer | `1000000` | `[1,...]` | X509IDMGMT_KAFKA_CONSUMER_MAX_IN_FLIGHT_REQ_PER_CONN | The maximum number of unacknowledged requests the client will send on a single connection before blocking. See more [here](https://kafka.apache.org/documentation/#producerconfigs_max.in.flight.requests.per.connection). |
| kafka.consumer.metadata.broker.list | string | `127.0.0.1:9092` | - | X509IDMGMT_KAFKA_CONSUMER_METADATA_BROKER_LIST | A comma-separated list containing the kafka's `host:port`. |
| kafka.consumer.socket.keepalive.enable | boolean | `false` | `true` or `false` | X509IDMGMT_KAFKA_CONSUMER_SOCKET_KEEPALIVE_ENABLE | Flag indicating whether the consumer socket should be kept active. |
| kafka.consumer.commit.interval.ms | integer | `5000` | positive integer | X509IDMGMT_KAFKA_CONSUMER_COMMIT_INTERVAL_MS | The frequency in milliseconds with which to save the position of the processor. See more [here](https://kafka.apache.org/documentation/#streamsconfigs_commit.interval.ms). |
| kafka.consumer.inprocess.max.msg | integer | `1` | - | X509IDMGMT_KAFKA_CONSUMER_INPROCESS_MAX_MSG | maximum number of messages in processing. |
| kafka.consumer.queued.max.msg.bytes | integer | `10485760` | `[0,...]` | X509IDMGMT_KAFKA_CONSUMER_QUEUED_MAX_MSG_BYTES | The largest record batch size allowed by Kafka. See more [here](https://kafka.apache.org/documentation/#topicconfigs_max.message.bytes). |
| kafka.consumer.subscription.backoff.min.ms | integer | `1000` (1 second) | `[0,...]` | X509IDMGMT_KAFKA_CONSUMER_SUBSCRIPTION_BACKOFF_MIN_MS | Default value (in milliseconds) for the initial backoff time implemented by the subscription mechanism. |
| kafka.consumer.subscription.backoff.max.ms | integer | `60000` (60 seconds) | `[0,...]` | X509IDMGMT_KAFKA_CONSUMER_SUBSCRIPTION_BACKOFF_MAX_MS | Default value (in milliseconds) for the maximum backoff time implemented by the subscription mechanism. |
| kafka.consumer.subscription.backoff.delta.ms | integer | `1000` (1 second) | `[0,...]` | X509IDMGMT_KAFKA_CONSUMER_SUBSCRIPTION_BACKOFF_DELTA_MS | Default value (in milliseconds) for the delta backoff time implemented by the subscription mechanism. |


#### _Device Manager_ Integration Settings

| Key | type | Default Value | Valid Values | Environment variable | Purpose |
|-----|------|---------------|--------------|----------------------|---------|
| devicemgr.device.url | string | `http://127.0.0.1:5000/device` | valid URL address | X509IDMGMT_DEVICEMGR_DEVICE_URL | URL address of the _Device Manager_ service to obtain information about the devices. |
| devicemgr.device.timeout.ms | integer | `10000` (10 seconds) | `[0,...]` | X509IDMGMT_DEVICEMGR_DEVICE_TIMEOUT_MS | _Device Manager_ response timeout |
| devicemgr.kafka.consumer.topic.suffix | string | `dojot.device-manager.device` | - | X509IDMGMT_DEVICEMGR_KAFKA_CONSUMER_TOPIC_SUFFIX | Suffix of the same topics that _Device Manager_ publishes data according to its events. |
| devicemgr.healthcheck.ms | integer | `10000` (10 seconds) | `[0,...]` | X509IDMGMT_DEVICEMGR_HEALTHCHECK_MS | Time interval in which the Kafka consumer who consumes _Device Manager_ events is checked to see if he is healthy |


#### Certificate Service Settings

| Key | type | Default Value | Valid Values | Environment variable | Purpose |
|-----|------|---------------|--------------|----------------------|---------|
| certservice.check.device.exists | boolean | `true` | `true` or `false` | X509IDMGMT_CERTSERVICE_CHECK_DEVICE_EXISTS | Flag indicating whether the device's existence check should be performed or not. |


### How to run

Beforehand, you need an already running dojot instance in your machine. Check
out the [dojot documentation](https://dojotdocs.readthedocs.io) for more
information on installation methods.

Generate the Docker image:

~~~shell
docker build -t <username>/x509-identity-mgmt:<tag> -f  .
~~~

Then an image tagged as `<username>/x509-identity-mgmt:<tag>` will be made
available. You can send it to your DockerHub registry to made it available for
non-local dojot installations:

~~~shell
docker push <username>/x509-identity-mgmt:<tag>
~~~

__NOTE THAT__  you can use the official image provided by dojot in its
[DockerHub page](https://hub.docker.com/r/dojot/x509-identity-mgmt).

## Debugging the service

To debug the service in a development environment, we prefer to use the VS Code,
and for that there is the [.vscode/launch.json](./js/.vscode/launch.json) file
that can be studied and there you will have tips on what you need to be present
in your local environment to run the service outside the container.

See also the [default.conf](./js/config/default.conf) file used by the service,
look for URLs there and you will have a good tip of what needs to be
externalized from the _docker-compose_ environment for the service to have
access. The [development.conf](./js/config/development.conf) file takes
precedence over the [default.conf](./js/config/default.conf) file when we run
the _service_ in _debug mode_ using VS Code but it is completely discarded in a
_production_ environment.

Basically, you must attend to the _version_ of Node.js and have installed the
_build-essential_ library (in the case of Linux distributions based on Debian).
It is likely that in order to compile the `node-rdkafka` _node_module_ it will
be necessary to install the _gzip compression_ library. In this case, the most
advisable is to study the dependencies declared in the [Dockerfile](./Dockerfile)
file to have an idea of what is needed.

You may need the following dependencies installed on your Linux:
~~~shell
$ # in the case of Linux distributions based on Debian:
$ sudo apt-get install -y \
               build-essential \
               node-gyp \
               make \
               ca-certificates \
               gzip
~~~

For the _x509-identity-mgmt_ service to be able to access the other services on
the dojot platform, it is necessary to map the service container _ports_ to
ports on your _localhost_ (based on a _docker-compose_ deployment).
See the [development.conf](./js/config/development.conf) file for dependent
services.

In order for the service (running locally) to be able to connect to Kafka
(inside the docker-compose), in addition to externalizing the Kafka container
port to your localhost, it is also necessary to edit the `/etc/hosts` file on
your Linux:
~~~shell
$ sudo vi /etc/hosts
~~~

And include the following entry:

~~~
127.0.0.1 kafka
~~~

This way, your local DNS will know how to correctly resolve the domain for the
_kafka_ service (remember that it is necessary to externalize the Kafka
container port to your localhost).

In order for the service (running locally) to connect to the EJBCA container,
it is necessary to obtain the access files generated by the EJBCA, for this you
will have to copy the files from the _docker volume_ to your local development
directory:

~~~shell
$ EJBCA_CONTAINER_ID=$(docker ps --quiet --filter "label=com.docker.compose.service=x509-ejbca")

$ docker cp ${EJBCA_CONTAINER_ID}:/opt/tls .
~~~

Assuming that your current directory is the _root_ of the project, that is, the
same directory as the `package.json` file, The `./tls` subdirectory will be
created and the files needed to connect to the EJBCA container will be inside.
Remembering that for this to work, it is necessary to use dojot's
`docker-compose` deployment.


## Testing the service

To perform unit tests, just open a terminal in the project's root directory
(the same directory as the `package.json` file) and execute the following
command:

~~~shell
$ npm test
~~~

Unit tests will be performed and a test coverage report will be generated in
the `./coverage` directory.
To view the test coverage report, simply open the
`./coverage/lcov-report/index.html` file in a browser.

To debug the unit tests, run the command:

~~~shell
$ npm run debugtest
~~~

From there, you can use the VS Code to attach to the running process and debug
the test cases. Learn more at [Jest Troubleshooting](https://jestjs.io/docs/troubleshooting#debugging-in-vs-code).

## Documentation

Check the documentation for more information:

- [Latest *x509-identity-mgmt* API documentation](https://dojot.github.io/dojot/x509-identity-mgmt/apiary_latest.html)
- [Development *x509-identity-mgmt* API documentation](https://dojot.github.io/dojot/x509-identity-mgmt/apiary_development.html)
- [Latest dojot platform documentation](https://dojotdocs.readthedocs.io/en/latest)

## Issues and help

If you found a problem or need help, leave an issue in the main
[dojot repository](https://github.com/dojot/dojot) and we will help you!