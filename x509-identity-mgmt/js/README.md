# Service x509-identity-mgmt (Node.js Application)

## Component settings

Settings of the Node.js application that implements the business rules of the x.509 identity component.

| Variable | Description |
|----------|-------------|
| NODE_ENV | Is used (by convention) to state whether a particular environment is a **production** or a **development** environment. <br>**Default**: production |
| NODE_APP_PORT | Port on which the application listens for http requests.<br>**Default**: 3000 |
| TRUST_PROXY | By enabling the trust proxy feature, the IP address will be updated in the correct places with the forwarded IP. It will also be available on the *req* object. Note: If the app is running behind a reverse proxy, it is necessary to configure the reverse proxy to forward the client's real IP address. <br><br>Enabling trust proxy will have the following impact:<br><br> - The value of *req.hostname* is derived from the value set in the *X-Forwarded-Host* header, which can be set by the client or by the proxy;<br><br> - *X-Forwarded-Proto* can be set by the reverse proxy to tell the app whether it is https or http or even an invalid name. This value is reflected by *req.protocol*;<br><br> - The *req.ip* and *req.ips* values are populated with the list of addresses from *X-Forwarded-For*.<br><br>See for [express.js](http://expressjs.com/en/guide/behind-proxies.html) more details.<br><br>**Default**: false |
| REQ_MAX_BODY_SIZE | Express [BodyParser](https://expressjs.com/en/resources/middleware/body-parser.html) *limit* setting.<br>**Default**: 100kb |
| REQ_PGNT_LIMIT | Express [Paginate](https://github.com/expressjs/express-paginate#arguments) *limit* setting.<br>**Default**: 25 |
| REQ_PGNT_MAX_LIMIT | Express [Paginate](https://github.com/expressjs/express-paginate#arguments) *max limit* setting.<br>**Default**: 250 |
| TERMINUS_VERBATIM | [Terminus](https://github.com/godaddy/terminus) *verbatim* setting, used to return the custom object from */healthcheck* in response.<br>**Default**: true |
| TERMINUS_TIMEOUT_MS | [Terminus](https://github.com/godaddy/terminus) *timeout* setting, define the number of milliseconds before forceful exiting.<br>**Default**: 5000 |
| TERMINUS_SIGNALS | [Terminus](https://github.com/godaddy/terminus) *signals* setting, signals to listen for relative to shutdown. Must be separated by commas.<br>**Default**: SIGINT,SIGTERM |
| EJBCA_HEALTHCHECK | EJBCA Server Health Check URL. <br>Useful when in a development environment.<br>**Default**: http://127.0.0.1:8080/ejbca/publicweb/healthcheck/ejbcahealth' |
| EJBCA_WSDL | URL for the WSDL that describes the operations available on the EJBCA via SOAP.<br>Useful when in the development environment. <br>**Default**: https://127.0.0.1:8443/ejbca/ejbcaws/ejbcaws?wsdl |
| EJBCA_TLS_CLIENT_DIR | Directory where the [PKCS#12](https://en.wikipedia.org/wiki/PKCS_12) file will be generated so that the client application can establish communication with the EJBCA server. <br>**Default**: /opt/tls |
| EJBCA_CLIENT_USERNAME | This value is used to create an End Entity in the EJBCA, as well as to name the file [PKCS#12](https://en.wikipedia.org/wiki/PKCS_12) used by the client application to communicate with the EJBCA server via SOAP. <br>**Default**: ejbcaclient |
| EJBCA_CRL_FORCE_RENEW | Forces CRL renewal every time it is requested.<br>**Default**: false |
| EJBCA_DEVICES_CA | Name of the CA that signs certificates for IoT devices. This value is also added in the SubjectDN (Common Name) field of this CA's certificate. <br>**Default**: X509 Identity CA |
| EJBCA_SERVICES_CA | Name of the CA that signs certificates for dojot platform services. This value is also added in the SubjectDN (Common Name) field of this CA's certificate. <br>**Default**: Services CA |
| CERT_ALW_ATTR | SubjectDN fields allowed to be entered by the user in the CSR when requesting a certificate for an IoT device. The list of fields must be separated by commas.<br>**Default**: CN |
| CERT_ALW_ATTR_REGEX | Constraints ([RegExp](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/RegExp)) on the values of the allowed SubjectDN fields. <br>The values must represent a map (key = value), the separator of map entries being the semicolon. <br>Exemple: 'C = ^\[A-Z\]{2,3}\$ ; ST = ^\[A-Za-z \]{1,50}\$' <br>This means that the value for the *Country* field is limited to the regular expression: ^\[A-Z\]{2,3}\$ <br>Also, the *State* field is limited to the regular expression: ^\[A-Za-z \]{1,50}\$ |
| CERT_DN_O | In particular, the SubjectDN *Organization* field has a constant value and cannot be changed by a CSR.<br>**Default**: dojot IoT Platform |
| CERT_VALIDITY_DAYS | certificate validity (in days).<br>**Default**: 365 |
| CERT_CHECK_PUBLIC_KEY | Flag indicating whether the CSR public key validation should be performed or not.<br>**Default**: true |
| MONGO_URI | MongoDB [Connection String](https://docs.mongodb.com/manual/reference/connection-string/).<br>**Default**: mongodb://127.0.0.1:27017/x509-identity-mgmt |
| MONGO_USER | The username for auth (e.g. 'root').<br>**Default**: undefined |
| MONGO_PASS | The password for auth (e.g. 'pass').<br>**Default**: undefined |
| MONGO_AUTH_DB | Define the database to authenticate against (e.g. 'admin').<br>**Default**: undefined |
| MONGO_AUTO_IDX | By default, [mongoose](https://mongoosejs.com/docs/connections.html#options) will automatically build indexes defined in the schemas when it connects. This is great for development, but not ideal for large production deployments (where a more improved DB update strategy should be employed), because index builds can cause performance degradation. If you set autoIndex to false, mongoose will not automatically build indexes for any model.<br>**Default**: true |
| MONGO_POOL | The maximum number of sockets the [MongoDB driver](http://mongodb.github.io/node-mongodb-native/3.5/api/MongoClient.html#.connect) will keep open.<br>**Default**: 100 |
| MONGO_CONN_TIMEOUT_MS | How long (in milliseconds) to wait for a connection to be established before timing out.<br>**Default**: 30000 |
| MONGO_SOCKET_TIMEOUT_MS | How long (in milliseconds) a send or receive on a socket can take before timing out.<br>**Default**: 360000 |
| MONGO_HB_FREQ_MS | controls when the [MongoDB driver](http://mongodb.github.io/node-mongodb-native/3.5/api/MongoClient.html#.connect) checks the state of the MongoDB deployment (heartbeat Frequency).<br>Specify the interval (in milliseconds) between checks.<br>**Default**: 10000 |
| MONGO_CONN_IPV | Version of IP stack. Can be 4, 6 or 0.<br>If 0, will attempt to connect with IPv6, and will fall back to IPv4 on failure.<br>**Default**: 0 |
| MONGO_QUERY_MAX_TIME_MS | Sets the maxTimeMS option. This will tell the MongoDB server to abort if the query or write op has been running for more than ms milliseconds.<br>**Default**: 30000 |
