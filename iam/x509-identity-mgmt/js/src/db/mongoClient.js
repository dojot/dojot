const mongoose = require('mongoose');

/**
 * To fix all deprecation warnings:
 * https://mongoosejs.com/docs/deprecations.html
 */
mongoose.set('useNewUrlParser', true);
mongoose.set('useFindAndModify', false);
mongoose.set('useCreateIndex', true);
mongoose.set('useUnifiedTopology', true);

function createObject(
  config, healthCheck, logger, errorTemplate,
) {
  const { BadRequest } = errorTemplate;

  function parseProjectionFields(commaSeparatedFields, projectableFields) {
    const queryFields = (commaSeparatedFields)
      ? commaSeparatedFields.split(',')
      : null;

    /* Defines the fields that will be returned by the query */
    let fields = [...projectableFields];
    if (queryFields) {
      const invalidFields = queryFields.filter((el) => !projectableFields.includes(el));
      if (invalidFields.length) {
        const errorMsg = `The fields provided are not valid: [${invalidFields.join(',')}]`;
        throw BadRequest(errorMsg);
      }
      fields = [...queryFields];
    }

    // you cannot specify an embedded document and a field
    // within that document embedded in the same projection
    fields = fields.filter((fname) => !fields.some((other) => other.startsWith(`${fname}.`)));

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

  /* Flag indicating whether the MongoDB Driver had the first successful connection */
  let initialized = false;

  const connOpts = {
    user: config.options.user,
    pass: config.options.pass,
    authSource: config.options.authsource,
    autoIndex: config.options.autoindex,
    poolSize: config.options.poolsize,
    serverSelectionTimeoutMS: config.options.serverselectiontimeoutms,
    heartbeatFrequencyMS: config.options.heartbeatfrequencyms,
    socketTimeoutMS: config.options.sockettimeoutms,
    family: config.options.family,
  };

  const connect = () => {
    logger.info('Establishing connection with MongoDB');
    return mongoose.connect(config.uri, connOpts)
      .catch((err) => {
        logger.error(`Mongoose connect() failed with error: ${err.message}`);
      });
  };

  /* Emitted when an error occurs on this connection. */
  mongoose.connection.on('error', (err) => {
    logger.error(err);
  });

  /* Emitted when this connection successfully connects to the db.
   * May be emitted multiple times in reconnected scenarios. */
  mongoose.connection.on('connected', () => {
    logger.info('Connection established to MongoDB');
    initialized = true;
    healthCheck.ready();
  });

  /* Emitted when Mongoose lost connection to the MongoDB server. This event may be
   * due to your code explicitly closing the connection, the database server crashing,
   * or network connectivity issues. */
  mongoose.connection.on('disconnected', () => {
    logger.error('Lost MongoDB connection');
    healthCheck.notReady();
    if (!initialized) {
      setTimeout(() => connect(), config.options.heartbeatFrequencyMS);
    }
  });

  /* Emitted if Mongoose lost connectivity to MongoDB and successfully reconnected.
   * Mongoose attempts to automatically reconnect when it loses connection to the database. */
  mongoose.connection.on('reconnected', () => {
    logger.info('Reconnected to MongoDB');
    healthCheck.ready();
  });

  /* Emitted when you're connected to a standalone server and Mongoose has run out
   * of reconnectTries. The MongoDB driver will no longer attempt to reconnect after
   * this event is emitted. This event will never be emitted if you're connected
   * to a replica set. */
  mongoose.connection.on('reconnectFailed', () => {
    logger.error('Reconnected to MongoDB failed');
    initialized = false;
    healthCheck.notReady();
    setTimeout(() => connect(), config.options.heartbeatFrequencyMS);
  });

  /* Emitted when you're connecting to a replica set and Mongoose has successfully
   * connected to the primary and at least one secondary. */
  mongoose.connection.on('fullsetup', () => {
    logger.info('Connected to the primary and at least one secondary server on the MongoDB Replica Set');
    healthCheck.ready();
  });

  /* Emitted when you're connecting to a replica set and Mongoose has successfully
   * connected to all servers specified in your connection string. */
  mongoose.connection.on('all', () => {
    logger.info('Connected to all servers on the MongoDB Replica Set');
    healthCheck.ready();
  });

  mongoose.connection.on('close', () => {
    logger.info('MongoDB connection has been closed');
    initialized = false;
    healthCheck.notReady();
  });

  return {
    connect,
    close: mongoose.connection.close.bind(mongoose.connection),
    parseProjectionFields,
    sanitizeFields,
  };
}

module.exports = ({
  config, healthCheck, logger, errorTemplate,
}) => createObject(
  config, healthCheck, logger, errorTemplate,
);
