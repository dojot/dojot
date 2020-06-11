const mongoose = require('mongoose');

const MongoQS = require('mongo-querystring');

const { Logger } = require('@dojot/microservice-sdk');

const { mongo: { conn: connCfg } } = require('./config');

const { Schema } = mongoose;

const logger = new Logger();

const on = mongoose.connection.on.bind(mongoose.connection);

/* Flag indicating whether the MongoDB Driver had the first successful connection */
let initialized = false;

/* flag with the health check result of the database connection */
let healthCheck = false;

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

const connect = () => {
  logger.info('Establishing connection with MongoDB');
  return mongoose.connect(connCfg.uri, connCfg.options)
    .catch((err) => {
      logger.error(`Mongoose connect() failed with error: ${err.message}`);
    });
};

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


/** ************************************************************************
 * Customizations used to select certificate fields from the QueryString URL
 ************************************************************************* */
const certificateProjectableFields = [
  'fingerprint',
  'pem',
  'createdAt',
  'issuedByDojotPki',
  'autoRegistered',
  'belongsTo.device',
  'belongsTo.application',
  'tenant',
];

function getProjectionFields(commaSeparatedFields) {
  const queryFields = (commaSeparatedFields)
    ? commaSeparatedFields.split(',')
    : null;

  /* Defines the fields that will be returned by the query */
  const fields = (queryFields)
    ? certificateProjectableFields.filter(
      (path) => queryFields.some(
        (field) => path.startsWith(field),
      ),
    )
    : [...certificateProjectableFields];

  /* _id must be specifically excluded ('-' prefix) */
  fields.push('-_id');

  return fields;
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

function getConditionFields(urlQueryStringObj, tenant) {
  const mongodbFilterObj = certificateQueryString.parse(urlQueryStringObj);
  return Object.assign(mongodbFilterObj, { tenant });
}
/** ***************************************************************** */

module.exports = {
  on,
  connect,
  healthCheck: () => healthCheck,
  close: mongoose.connection.close.bind(mongoose.connection),
  certificate: {
    model: mongoose.model('Certificate', certificateSchema),
    getProjectionFields,
    getConditionFields,
  },
};
