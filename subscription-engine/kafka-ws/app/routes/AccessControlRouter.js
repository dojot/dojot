const express = require('express');
const jwt = require('express-jwt');
const unless = require('express-unless');
const createError = require('http-errors');
const service = require('../services/TicketService');

const { ConfigManager } = require('@dojot/microservice-sdk');
const KAFKA_WS_CONFIG_LABEL = 'KAFKA_WS';

const config = ConfigManager.getConfig(KAFKA_WS_CONFIG_LABEL).ticket;

/* The "/ticket" route (as well as "/public" route)
 * should not require a ticket to be accessed, so skip
 * the middleware when this condition is met. */
const skipWhen = { path: /^.+\/(ticket|public)$/ig };

module.exports = () => {
  const router = express.Router();

  /* Rips the "one-time" ticket, so it won't be used again */
  const ticketRipperMiddleware = async (req, res, next) => {
    /* If the component is configured to require tickets,
     * it must be informed via QueryString */
    if (!req.query.ticket) {
      const err = new createError.Unauthorized();
      err.message = 'Missing ticket';
      return next(err);
    }

    /* The ticket is opaque, but it allows us to have access
     * to details that do not travel through the network.
     * These details are contained in a JWT token, this token
     * is like a transparent version of the ticket, in which
     * we can read its content.
     * The token can only be obtained once, after which it is
     * removed from the database. Thus, it is assumed that
     * the ticket has been "ripped". */
    const token = await service.retrieveEncodedToken(req.query.ticket);

    /* If the token is no longer in the database, the ticket
     * may have already been used once or it has expired.
     * In any case, this ticket is invalid! */
    if (!token) {
      const err = new createError.Unauthorized();
      err.message = 'Invalid ticket';
      return next(err);
    }

    /* adds the token in the request header and moves on to
     * the next middleware... */
    req.headers.authorization = `Bearer ${token}`;
    return next();
  };
  ticketRipperMiddleware.unless = unless;

  /* Performs introspection of the ticket-related JWT token
   * and makes it available to the next middlewares */
  const tokenIntrospectionMiddleware = jwt({
    secret: config.secret,
    requestProperty: 'token',
  });

  router.use(
    ticketRipperMiddleware.unless(skipWhen),
    tokenIntrospectionMiddleware.unless(skipWhen),
  );

  return router;
};
