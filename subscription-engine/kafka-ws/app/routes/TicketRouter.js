const express = require('express');
const jwtDecode = require('jwt-decode');
const createError = require('http-errors');
const HttpStatus = require('http-status-codes');
const service = require('../services/TicketService');

/* This route is responsible for issuing new tickets */
const ticketingRoute = '/ticket';

module.exports = () => {
  const router = express.Router();

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
          if (payload.iss && payload.exp) {
            req.auth = {
              tenant: payload.iss.substring(payload.iss.lastIndexOf('/') + 1),
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
