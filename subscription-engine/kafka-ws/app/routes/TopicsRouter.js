const express = require('express');

const websocketTarball = require('../WebsocketTarball');

module.exports = () => {
  const router = express.Router();

  router.ws('/topics/:topic', async (ws, req) => {
    const params = {
      ws,
      connection: req.connection,
      token: req.token,
      topic: req.params.topic,
      fields: req.query.fields,
      where: req.query.where,
    };
    await websocketTarball.onConnection(params);
  });

  return router;
};
