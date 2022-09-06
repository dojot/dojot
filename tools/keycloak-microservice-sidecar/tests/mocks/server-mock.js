const express = require('express');

module.exports = function createMockServer(port = 5001) {
  const app = express();

  app.get('/test', async (req, res) => {

    res.status(200).send();
  });

  // Init http server
  return app.listen(port, () => {
    console.log(`http://0.0.0.0:${port} Demo API`);
  });
}