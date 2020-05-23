const path = require('path');

/* parse unsigned integer */
const parseUint = (val) => val && Math.abs(parseInt(val, 10));

/* parse boolean */
const parseBool = (val, def = false) => {
  if (typeof val === 'string') {
    if (val.toUpperCase() === 'TRUE' || val === '1') return true;
    if (val.toUpperCase() === 'FALSE' || val === '0') return false;
  }
  return def;
};

/* parse array */
const parseArray = (val) => {
  if (typeof val !== 'string') return undefined;
  return val.split(',').map((el) => el.trim());
};

/* parse regex validator for values in SubjectDN allowed attributes */
const parseAllowedAttrsRegex = (val) => {
  if (typeof val !== 'string') return undefined;
  return val.split(';').reduce((obj, attr) => {
    const keyValue = attr.split('=').map((el) => el.trim());
    const key = keyValue[0];
    const value = new RegExp(keyValue[1]);
    if (!Reflect.has(obj, key)) {
      Reflect.set(obj, key, value);
    }
    return obj;
  }, {});
};

module.exports = {

  nodeEnv: process.env.NODE_ENV || 'production',

  /* HTTP Server settings */
  server: {
    port: parseUint(process.env.NODE_APP_PORT) || 3000,
    apiURL: process.env.EJBCA_API_URL || 'https://dojot.github.io/ejbca-rest/apiary_latest.html',
  },

  /* By enabling the trust proxy feature, the IP address will be
   * updated in the correct places with the forwarded IP.
   * It will also be available on the req object.
   * Note: If the app is running behind a reverse proxy, it is
   * necessary to configure the reverse proxy to forward the
   * client's real IP address.
   * Enabling trust proxy will have the following impact:
   * -> The value of req.hostname is derived from the value set in
   *    the X-Forwarded-Host header, which can be set by the client
   *    or by the proxy.
   * -> X-Forwarded-Proto can be set by the reverse proxy to tell
   *    the app whether it is https or http or even an invalid name.
   *    This value is reflected by req.protocol.
   * -> The req.ip and req.ips values are populated with the list of
   *    addresses from X-Forwarded-For.
   * http://expressjs.com/en/guide/behind-proxies.html */
  trustProxy: parseBool(process.env.TRUST_PROXY),

  /* Log settings */
  logFormat: ([
    ':remote-addr',
    '-',
    ':remote-user',
    ':id',
    '":method :url HTTP/:http-version"',
    ':status',
    ':res[content-length]',
    '":referrer"',
    '":user-agent"',
  ]).join(' '),
  logToFile: parseBool(process.env.USE_LOG_FILE),
  rotatingFileStream: {
    filename: 'access.log',
    options: {
      size: '10M', // rotate every 10 MegaBytes written
      interval: '1d', // rotate daily
      maxFiles: 30, // the maximum number of rotated files to be kept
      compress: 'gzip', // compress rotated files
      path: path.join(__dirname, 'log'),
    },
  },

  /* Express BodyParser settings */
  bodyParser: {
    limit: process.env.REQ_MAX_BODY_SIZE || '100kb',
  },

  /* Express Paginate settings */
  paginate: {
    limit: parseUint(process.env.REQ_PGNT_LIMIT) || 25,
    maxLimit: parseUint(process.env.REQ_PGNT_MAX_LIMIT) || 250,
  },

  /* https://github.com/godaddy/terminus settings */
  terminus: {
    /* use object returned from /healthcheck in response */
    verbatim: parseBool(process.env.TRMNS_VERBTM, true),
    /* number of milliseconds before forceful exiting */
    timeout: parseUint(process.env.TRMNS_TMOUT) || 5000,
    /* array of signals to listen for relative to shutdown */
    signals: parseArray(process.env.TRMNS_SIGNLS) || ['SIGINT', 'SIGTERM'],
  },

  /* EJBCA Integration settings */
  ejbca: {
    healthCheck: process.env.EJBCA_HEALTHCHECK || 'http://127.0.0.1:8080/ejbca/publicweb/healthcheck/ejbcahealth',

    wsdl: process.env.EJBCA_WSDL || 'https://127.0.0.1:8443/ejbca/ejbcaws/ejbcaws?wsdl',

    pkcs12: (process.env.EJBCA_TLS_CLIENT_DIR
      ? `${process.env.EJBCA_TLS_CLIENT_DIR}/${process.env.EJBCA_CLIENT_USERNAME}.p12`
      : '/opt/tls/ejbcaclient.p12'),

    pkcs12secret: (process.env.EJBCA_TLS_CLIENT_DIR
      ? `${process.env.EJBCA_TLS_CLIENT_DIR}/${process.env.EJBCA_CLIENT_USERNAME}.secret`
      : '/opt/tls/ejbcaclient.secret'),

    trustedCA: (process.env.EJBCA_TLS_CLIENT_DIR
      ? `${process.env.EJBCA_TLS_CLIENT_DIR}/${process.env.EJBCA_CLIENT_USERNAME}-trustedca.pem`
      : '/opt/tls/ejbcaclient-trustedca.pem'),

    forceCRLCreation: parseBool(process.env.EJBCA_CRL_FORCE_CREATION),

    rootCA: process.env.EJBCA_DEVICES_CA || 'X509 Identity CA',

    innerRootCA: process.env.EJBCA_SERVICES_CA || 'Services CA',
  },

  /* Settings related to certificate control */
  certificate: {
    subject: {
      allowedAttrs: parseArray(process.env.CERT_ALW_ATTR) || ['CN'],

      /* constraints on the values of the allowed attributes on SubjectDN */
      allowedAttrsConstraints: parseAllowedAttrsRegex(process.env.CERT_ALW_ATTR_REGEX) || {
        CN: /^[0-9A-Za-z ]{1,255}$/,
      },

      mandatoryAttrs: parseArray(process.env.CERT_MDT_ATTR) || ['CN'],

      constantAttrs: {
        O: process.env.CERT_DN_O || 'dojot IoT Platform',
      },
    },
    /* Positive integer - certificate validity (in days) */
    validity: parseUint(process.env.CERT_VALIDITY_DAYS) || 365,
    checkPublicKey: parseBool(process.env.CERT_CHECK_PUBLIC_KEY, true),
  },

  /*
   * Settings related to mongoDBClient/mongoose connections/queries:
   * http://mongodb.github.io/node-mongodb-native/3.5/api/MongoClient.html#.connect
   * https://mongoosejs.com/docs/connections.html
   */
  mongo: {
    conn: {
      /* https://docs.mongodb.com/manual/reference/connection-string/ */
      uri: process.env.MONGO_URI || 'mongodb://127.0.0.1:27017/x509-identity-mgmt',
      options: {

        /* The username for auth */
        user: process.env.MONGO_USER || 'root',

        /* The password for auth */
        pass: process.env.MONGO_PASS || 'pass',

        /* Define the database to authenticate against */
        authSource: process.env.MONGO_AUTH_DB || 'admin',

        /* By default, mongoose will automatically build indexes defined in
         * your schema when it connects. This is great for development, but
         * not ideal for large production deployments, because index builds
         * can cause performance degradation. If you set autoIndex to false,
         * mongoose will not automatically build indexes for any model */
        autoIndex: parseBool(process.env.MONGO_AUTO_IDX, true),

        /* The maximum number of sockets the MongoDB driver will keep open */
        poolSize: parseUint(process.env.MONGO_POOL) || 100,

        /* How long to wait for a connection to be established before timing out */
        serverSelectionTimeoutMS: parseUint(process.env.MONGO_CONN_TIMEOUT) || 30000,

        /* controls when the driver checks the state of the MongoDB deployment.
         * Specify the interval (in milliseconds) between checks */
        heartbeatFrequencyMS: parseUint(process.env.MONGO_HB_FREQ) || 10000,

        /* How long a send or receive on a socket can take before timing out */
        socketTimeoutMS: parseUint(process.env.MONGO_SOCKET_TIMEOUT) || 360000,

        /* Version of IP stack. Can be 4, 6 or 0 (default).
         * if 0, will attempt to connect with IPv6, and
         * will fall back to IPv4 on failure */
        family: parseUint(process.env.MONGO_CONN_IPV) || 0,
      },
    },
    query: {
      /* Sets the maxTimeMS option. This will tell the MongoDB server to abort
       * if the query or write op has been running for more than ms milliseconds. */
      maxTimeMS: parseUint(process.env.MONGO_QUERY_MAX_TIME) || 30000,
    },
  },
};
