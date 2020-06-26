const express = require('express');

const websocketTarball = require('../WebsocketTarball');

module.exports = () => {
  const router = express.Router();

  router.ws('/topics/:topic', async (ws, req) => {
    const { topic } = req.params;
    const { fields, where } = req.query;
    await websocketTarball.onConnection(ws, req, topic, fields, where);
  });

  return router;
};
