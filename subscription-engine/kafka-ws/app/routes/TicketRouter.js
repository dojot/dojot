const express = require('express');

const jwtDecode = require('jwt-decode');

const createError = require('http-errors');

const HttpStatus = require('http-status-codes');

const service = require('../services/TicketService');

module.exports = () => {
  const router = express.Router();

  router.route('/ticket')
    /* To obtain a "ticket" that makes it possible to establish a
     * connection to the server via websocket, the user must be
     * authenticated on the platform, that is, an access token
     * (JWT) issued by the IAM is required.
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
