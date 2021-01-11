# CertGen

This is a scripting tool for generating certificates on disk for end-to-end testing and local development. It is responsible for creating a _Certificate Authority_ (CA) and using it to issue certificates for client and server applications.

To establish encrypted communication between the client and the server, it is necessary to use the TLS protocol and x.509 certificates, but some particularities of the certificates must be taken into account. Basically the functionality of this tool is to carefully generate certificates prepared for use in TLS connections between client and server, that is, the details of how the certificates are configured are the responsibility of this tool. What really matters to anyone using this tool is the _host name_ (or IP) of the server that the client application tries to connect to. It must match one of the _Subject Alternative Names_ in the certificate of the server (or, failing that, it must match the common name (CN) of the cert's Subject Distinguished Name (DN).

With this tool, just inform the host names, IPs and CN of the certificate as parameters and it takes care to generate a ready-to-use certificate.

It uses the [OpenSSL](https://www.openssl.org/) to generate certificates and cryptographic keys.

## Usage

This tool creates/uses the directories `./ca` and `./certs` to store the certificates and public/private keys of the CA and hosts respectively.

###### Example 1

Give the script execution permission (`chmod u+x certgen.sh`) and enter the command:

~~~bash
./certgen.sh \
    --dns 'server.domain.com' \
    --cname 'A-simple-Common-Name'
~~~

That done, the script will check if there is already a CA in the `./ca` directory to be used to issue the certificate to the host that we want to certify, in the case of the example above: `server.domain.com`.

If there is still no CA (in the case of the first execution), the script will generate the _self-signed_ certificate and the _public/private_ key pair for this CA, as well as the file for generating the serial numbers of the certificates it signs.

The host's certificate and key pair will be saved in the `./certs` directory. The file name will be based on the value of the `--cname` parameter.

###### Example 2

The following example explores the use of multiple DNS and IP entries in the _SubjectAltName_ (SAN) extension of the certificate to be generated.

~~~bash
./certgen.sh \
    --dns 'domain1.com,domain2.com,domain3.com' \
    --ip '192.168.0.10,192.168.0.20,192.168.0.30' \
    --cname 'CertificateCommonName'
~~~

This command will generate a certificate whose CNAME will be `CertificateCommonName` and the _SubjectAlternativeName_ extension will have DNS and IP comma-separated entries.

#### Parameters

key | value
--- | -----
--dns | Used to validate domain Ownership. Useful when the server is behind a proxy or load balancer, That is, you can create multiple certificates that have the same domain name.
--ip | Useful when there is a direct connection between the client and the server via IPs (without the need for domain resolution).
--cname | If the SAN extension is not present in the certificate, the CN field of the SubjectDN is used to validate the domain. This attribute is also used to name the generated file.

**Note:** When using the `--dns` attribute, ensure that the DNS Server contains the translation of the _hostname_ to the correct IP, otherwise the client will not be able to validate the server. As an alternative to DNS Server, it is possible to use the [_hosts_ file](https://www.howtogeek.com/howto/27350/beginner-geek-how-to-edit-your-hosts-file/) of the client operating system.

#### Environment variables

It is possible to make some configurations through environment variables, they must be exported in the current shell before executing the script:

key | value
--- | -----
CA_CNAME | Common Name of the CA certificate, this name doesn't really do much, but if you want to change that, it’s possible. **(*)**
CA_VALIDITY | A positive integer that defines the validity of the CA certificate in days. **(*)** <br>**Default:** 1825 days (5 years)
HOST_VALIDITY | A positive integer that defines the validity of the host's certificate in days. <br>**Default:** 365 days (1 year)

> **(*)** For this setting to take effect, there must be no certificate in the `./ca` directory.

## Lastly

With the CA certificate and the host's certificate and key pair, it is possible to run an HTTP server over the TLS protocol.

On the client side, the CA certificate _must_ be configured as trusted.
Once the client trusts the CA certificate, all host certificates issued by this tool will be accepted on the client (as long as the CA certificate is kept unchanged in the `./ca` directory).

Once the client and the server trust the same CA certificate, TLS will be established. Note that the CA's private key **must not** leave the `./ca` directory, only the certificate must be copied to the client and server. If a new CA certificate is generated, it will be necessary to replace all host certificates issued based on the previous CA certificate.