const jwt = require('jsonwebtoken');

const { app: appCfg } = require('../Config');

/**
 * Issue a ticket so that it is possible to establish
 * communication with the server via websocket.
 *
 * @param {Object} auth Object with data extracted from the user's access token.
 */
async function issueTicket({ tenant, expiration }) {
  const ticket = await new Promise((resolve, reject) => {
    jwt.sign({
      tenant, remainingTime: expiration,
    },
    appCfg.ticket.secret,
    { expiresIn: appCfg.ticket.expiresIn },
    (err, token) => {
      if (err) {
        reject(err);
      } else {
        resolve(token);
      }
    });
  });
  return ticket;
}

module.exports = {
  issueTicket,
};
