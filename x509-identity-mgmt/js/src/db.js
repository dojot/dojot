const mongoose = require('mongoose');

const MongoQS = require('mongo-querystring');

const { BadRequest } = require('./sdk/web/backing/error-template');

const { Schema } = mongoose;

/**
 * To fix all deprecation warnings:
 * https://mongoosejs.com/docs/deprecations.html
 */
mongoose.set('useNewUrlParser', true);
mongoose.set('useFindAndModify', false);
mongoose.set('useCreateIndex', true);
mongoose.set('useUnifiedTopology', true);

const certificateSchema = new Schema({
  fingerprint: String,
  pem: String,
  createdAt: { type: Date, default: Date.now },
  issuedByDojotPki: { type: Boolean, default: true },
  autoRegistered: { type: Boolean, default: false },
  belongsTo: new Schema({
    device: String,
    application: String,
  }),
  tenant: String,
});
certificateSchema.index({ tenant: 1, fingerprint: 1 });

/** ************************************************************************
 * Customizations used to select certificate fields from the QueryString URL
 ************************************************************************* */
const certProjectableFields = [
  'fingerprint',
  'pem',
  'createdAt',
  'issuedByDojotPki',
  'autoRegistered',
  'belongsTo',
  'belongsTo.device',
  'belongsTo.application',
  'tenant',
];

function parseProjectionFields(commaSeparatedFields) {
  const queryFields = (commaSeparatedFields)
    ? commaSeparatedFields.split(',')
    : null;

  /* Defines the fields that will be returned by the query */
  let fields = [...certProjectableFields];
  if (queryFields) {
    const invalidFields = queryFields.filter((el) => !certProjectableFields.includes(el));
    if (invalidFields.length) {
      const errorMsg = `The fields provided are not valid: [${invalidFields.join(',')}]`;
      throw BadRequest(errorMsg);
    }
    fields = [...queryFields];
  }
  return fields;
}

function getComplexFieldMap(dottedAllowedFields) {
  return dottedAllowedFields.reduce((map, el) => {
    const field = el.substring(0, el.indexOf('.'));
    if (field) {
      const remaining = el.substring(el.indexOf('.') + 1);
      let arr = map.get(field);
      if (!arr) {
        arr = [];
        map.set(field, arr);
      }
      arr.push(remaining);
    }
    return map;
  }, new Map());
}

function sanitizeFields(obj, dottedAllowedFields) {
  const objFields = Object.keys(obj);
  const allowedFields = new Set(dottedAllowedFields.map((el) => el.split('.')[0]));
  const notAllowedFields = objFields.filter((f) => !allowedFields.has(f));

  /* Removes non-allowed fields from the object */
  notAllowedFields.forEach((f) => Reflect.deleteProperty(obj, f));

  /* Sanitizes complex fields kept in the object */
  getComplexFieldMap(dottedAllowedFields).forEach((nestedAllowedFields, allowedField) => {
    if (Reflect.has(obj, allowedField)) {
      sanitizeFields(Reflect.get(obj, allowedField), nestedAllowedFields);
    }
  });

  return obj;
}

/** *******************************************************************
 * Customizations used to filter certificates from the QueryString URL
 ******************************************************************** */
const certificateQueryString = new MongoQS({
  keyRegex: /^[a-zA-Z0-9-_.]+$/i,
  arrRegex: /^[a-zA-Z0-9-_.]+(\[])?$/i,
  whitelist: {
    fingerprint: true,
    pem: true,
    createdAt: true,
    issuedByDojotPki: true,
    autoRegistered: true,
    'belongsTo.device': true,
    'belongsTo.application': true,
    tenant: true,
  },
});

function parseConditionFields(urlQueryStringObj, tenant) {
  const mongodbFilterObj = certificateQueryString.parse(urlQueryStringObj);
  return Object.assign(mongodbFilterObj, { tenant });
}
/** ***************************************************************** */

function initMongoClient(connCfg, logger) {
  /* Flag indicating whether the MongoDB Driver had the first successful connection */
  let initialized = false;

  /* flag with the health check result of the database connection */
  let healthCheck = false;

  const connOpts = {
    user: connCfg.options.user,
    pass: connCfg.options.pass,
    authSource: connCfg.options.authsource,
    autoIndex: connCfg.options.autoindex,
    poolSize: connCfg.options.poolsize,
    serverSelectionTimeoutMS: connCfg.options.serverselectiontimeoutms,
    heartbeatFrequencyMS: connCfg.options.heartbeatfrequencyms,
    socketTimeoutMS: connCfg.options.sockettimeoutms,
    family: connCfg.options.family,
  };

  const connect = () => {
    logger.info('Establishing connection with MongoDB');
    return mongoose.connect(connCfg.uri, connOpts)
      .catch((err) => {
        logger.error(`Mongoose connect() failed with error: ${err.message}`);
      });
  };

  const on = mongoose.connection.on.bind(mongoose.connection);

  /* Emitted when an error occurs on this connection. */
  on('error', (err) => {
    logger.error(err);
  });

  /* Emitted when this connection successfully connects to the db.
   * May be emitted multiple times in reconnected scenarios. */
  on('connected', () => {
    logger.info('Connection established to MongoDB');
    initialized = true;
    healthCheck = true;
  });

  /* Emitted when Mongoose lost connection to the MongoDB server. This event may be
   * due to your code explicitly closing the connection, the database server crashing,
   * or network connectivity issues. */
  on('disconnected', () => {
    logger.error('Lost MongoDB connection');
    healthCheck = false;
    if (!initialized) {
      setTimeout(() => connect(), connCfg.options.heartbeatFrequencyMS);
    }
  });

  /* Emitted if Mongoose lost connectivity to MongoDB and successfully reconnected.
   * Mongoose attempts to automatically reconnect when it loses connection to the database. */
  on('reconnected', () => {
    logger.info('Reconnected to MongoDB');
    healthCheck = true;
  });

  /* Emitted when you're connected to a standalone server and Mongoose has run out
   * of reconnectTries. The MongoDB driver will no longer attempt to reconnect after
   * this event is emitted. This event will never be emitted if you're connected
   * to a replica set. */
  on('reconnectFailed', () => {
    logger.error('Reconnected to MongoDB failed');
    initialized = false;
    healthCheck = false;
    setTimeout(() => connect(), connCfg.options.heartbeatFrequencyMS);
  });

  /* Emitted when you're connecting to a replica set and Mongoose has successfully
   * connected to the primary and at least one secondary. */
  on('fullsetup', () => {
    logger.info('Connected to the primary and at least one secondary server on the MongoDB Replica Set');
    healthCheck = true;
  });

  /* Emitted when you're connecting to a replica set and Mongoose has successfully
   * connected to all servers specified in your connection string. */
  on('all', () => {
    logger.info('Connected to all servers on the MongoDB Replica Set');
    healthCheck = true;
  });

  on('close', () => {
    logger.info('MongoDB connection has been closed');
    initialized = false;
    healthCheck = false;
  });

  return {
    connect,
    healthCheck: () => healthCheck,
    close: mongoose.connection.close.bind(mongoose.connection),
    certificate: {
      model: mongoose.model('Certificate', certificateSchema),
      parseProjectionFields,
      parseConditionFields,
      sanitizeFields: (cert) => sanitizeFields(cert, certProjectableFields),
    },
  };
}

module.exports = ({ config, logger }) => initMongoClient(config.conn, logger);
