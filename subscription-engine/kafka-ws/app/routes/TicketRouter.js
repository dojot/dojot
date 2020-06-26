const express = require('express');

const HttpStatus = require('http-status-codes');

const service = require('../services/TicketService');

module.exports = () => {
  const router = express.Router();
  router.route('/ticket')
  /* get a ticket to be able to connect to a route via websocket */
    .get(async (req, res) => {
      const ticket = await service.issueTicket(req.tenant);
      res.status(HttpStatus.OK).json({ ticket });
    });
  return router;
};
