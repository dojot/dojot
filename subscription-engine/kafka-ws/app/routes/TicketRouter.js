const express = require('express');

const jwt = require('express-jwt');

const jwtDecode = require('jwt-decode');

const unless = require('express-unless');

const createError = require('http-errors');

const HttpStatus = require('http-status-codes');

const service = require('../services/TicketService');

const { app: appCfg, server: servCfg } = require('./Config');

/* This route is responsible for issuing new tickets */
const ticketingRoute = '/ticket';

/* The "ticketing route" should not require a ticket to be accessed,
 * so skip a middleware when this condition is met. */
const skipWhen = { path: ticketingRoute };

module.exports = () => {
  const router = express.Router();

  /* Rips the "one-time" ticket, so it won't be used again */
  const ticketRipperMiddleware = async (req, res, next) => {
    /* If the component is configured to not require tickets,
     * it does not require this middleware. */
    if (!servCfg.requireTicket) {
      next();
    }

    /* If the component is configured to require tickets,
     * it must be informed via QueryString */
    if (!req.query.ticket) {
      const err = new createError.Unauthorized();
      err.message = 'Missing ticket';
      next(err);
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
      next(err);
    }

    /* adds the token in the request header and moves on to
     * the next middleware... */
    req.headers.authorization = `Bearer ${token}`;
    next();
  };
  ticketRipperMiddleware.unless = unless;

  /* Performs introspection of the ticket-related JWT token
   * and makes it available to the next middlewares */
  const tokenIntrospectionMiddleware = jwt({
    secret: appCfg.ticket.secret,
    credentialsRequired: servCfg.requireTicket,
    requestProperty: 'token',
  });

  router.use(
    ticketRipperMiddleware.unless(skipWhen),
    tokenIntrospectionMiddleware.unless(skipWhen),
  );

  router.route(ticketingRoute)
    /* To obtain a "ticket" that makes it possible to establish a
     * connection to the server via websocket, the user must be
     * authenticated on the platform, that is, an access token
     * (JWT) issued by the IAM is required.
     *
     * The JWT token in this case is not the one related to a ticket,
     * but the user-token that gives access to the platform!
     *
     * This middleware decodes the access token that must be sent
     * with the request. The token validation is not performed,
     * as it is expected to be validated by the API Gateway. */
    .all((req, res, next) => {
      const err = new createError.Unauthorized();
      if (req.headers.authorization) {
        const authHeader = req.headers.authorization.split(' ');
        if (authHeader.length === 2 && authHeader[0] === 'Bearer') {
          const token = authHeader[1];
          const payload = jwtDecode(token);
          if (payload.service && payload.exp) {
            req.auth = {
              tenant: payload.service,
              expiration: payload.exp,
            };
            return next();
          }
        }
        err.message = 'Invalid JWT token';
        return next(err);
      }
      err.message = 'Missing JWT token';
      return next(err);
    })
    /* get a ticket to be able to connect to a route via websocket */
    .get(async (req, res) => {
      const ticket = await service.issueTicket(req.auth);
      res.status(HttpStatus.OK).json({ ticket });
    });

  return router;
};
