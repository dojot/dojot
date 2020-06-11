const http = require('http');

const { Logger } = require('@dojot/microservice-sdk');

const cfg = require('./src/config');

const app = require('./src/app');

const db = require('./src/db');

const ejbca = require('./src/core/ejbca-facade');

const terminus = require('./src/terminus');

const server = http.createServer(app);

const logger = new Logger();

/* The server must be available even if there is not yet a connection to the database,
 * this is because it must be possible to consult the health check of the application
 * and it must cover the state of the connection to the database. */
server.listen(cfg.server.port, () => {
  logger.info('Server ready to accept connections');
  logger.info(server.address());
});

/* Starts the process of connecting to the database */
db.connect();

/* adds health checks and graceful shutdown to the application */
terminus.setup(server, db, ejbca);
