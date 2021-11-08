const {
  ConfigManager: { getConfig },
  Logger,
} = require('@dojot/microservice-sdk');
const { unflatten } = require('flat');
const mongoose = require('mongoose');
const { killApplication } = require('../Utils');

/**
 * To fix all deprecation warnings:
 * https://mongoosejs.com/docs/deprecations.html
 */
mongoose.set('useNewUrlParser', true);
mongoose.set('useFindAndModify', false);
mongoose.set('useCreateIndex', true);
mongoose.set('useUnifiedTopology', true);


const logger = new Logger('basic-auth:DB');

const config = unflatten(getConfig('BASIC_AUTH'));
const configMongo = config.mongo.conn;

/**
 * Class representing an MongoClient
 *
 * @class
 */
class MongoClient {
  /**
   * @constructor
   *
   * @param {an instance of @dojot/microservice-sdk.ServiceStateManager
   *          with register service 'basic-auth-db'} serviceState
   *          Manages the services' states, providing health check and shutdown utilities.
   */
  constructor(serviceState) {
    this.mongo = null;
    this.initialized = false;
    this.serviceState = serviceState;
  }

  /**
   *  Init new MongoClient instance
   */
  init() {
    try {
      this.connect();
      /* Emitted when an error occurs on this connection. */
      mongoose.connection.on('error', (err) => {
        logger.error(err);
      });

      /* Emitted when this connection successfully connects to the db.
       * May be emitted multiple times in reconnected scenarios. */
      mongoose.connection.on('connected', () => {
        logger.info('Connection established to MongoDB');
        this.initialized = true;
        this.serviceState.signalReady('basic-auth-db');
      });

      /* Emitted when Mongoose lost connection to the MongoDB server. This event may be
       * due to your code explicitly closing the connection, the database server crashing,
       * or network connectivity issues. */
      mongoose.connection.on('disconnected', () => {
        logger.error('Lost MongoDB connection');
        this.serviceState.signalNotReady('basic-auth-db');
        if (!this.initialized) {
          setTimeout(() => this.connect(), configMongo.options.heartbeatFrequencyMS);
        }
      });

      /* Emitted if Mongoose lost connectivity to MongoDB and successfully reconnected.
       * Mongoose attempts to automatically reconnect when it loses connection to the database. */
      mongoose.connection.on('reconnected', () => {
        logger.info('Reconnected to MongoDB');
        this.serviceState.signalReady('basic-auth-db');
      });

      /* Emitted when you're connected to a standalone server and Mongoose has run out
       * of reconnectTries. The MongoDB driver will no longer attempt to reconnect after
       * this event is emitted. This event will never be emitted if you're connected
       * to a replica set. */
      mongoose.connection.on('reconnectFailed', () => {
        logger.error('Reconnected to MongoDB failed');
        this.initialized = false;
        this.serviceState.signalNotReady('basic-auth-db');
        setTimeout(() => this.connect(), configMongo.options.heartbeatFrequencyMS);
      });

      /* Emitted when you're connecting to a replica set and Mongoose has successfully
       * connected to the primary and at least one secondary. */
      mongoose.connection.on('fullsetup', () => {
        logger.info('Connected to the primary and at least one secondary server on the MongoDB Replica Set');
        this.serviceState.signalReady('basic-auth-db');
      });

      /* Emitted when you're connecting to a replica set and Mongoose has successfully
       * connected to all servers specified in your connection string. */
      mongoose.connection.on('all', () => {
        logger.info('Connected to all servers on the MongoDB Replica Set');
        this.serviceState.signalReady('basic-auth-db');
      });

      mongoose.connection.on('close', () => {
        logger.info('MongoDB connection has been closed');
        this.initialized = false;
        this.serviceState.signalNotReady('basic-auth-db');
      });

      // registerShutdownHandler
      this.registerShutdown();
    } catch (error) {
      logger.error('There was an error creating the Cache.');
      logger.error(error.stack || error);
      killApplication();
    }
  }

  /**
   *  Connect to Mongo
   */
  async connect() {
    const connOpts = {
      user: configMongo.options.user,
      pass: configMongo.options.pass,
      authSource: configMongo.options.authsource,
      autoIndex: configMongo.options.autoindex,
      poolSize: configMongo.options.poolsize,
      serverSelectionTimeoutMS: configMongo.options.serverselectiontimeoutms,
      heartbeatFrequencyMS: configMongo.options.heartbeatfrequencyms,
      socketTimeoutMS: configMongo.options.sockettimeoutms,
      family: configMongo.options.family,
    };

    logger.info('Establishing connection with MongoDB');
    this.mongo = await mongoose.connect(configMongo.uri, connOpts)
      .catch((err) => {
        logger.error(`Mongoose connect() failed with error: ${err.message}`);
      });
  }

  /**
   *  Register 'shutdown' for MongoClient
   */
  registerShutdown() {
    this.serviceState.registerShutdownHandler(async () => {
      logger.warn(
        'Close all connections in MongoClient',
      );
      return this.mongo.disconnect();
    });
  }
}

module.exports = MongoClient;
