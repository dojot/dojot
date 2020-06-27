const crypto = require('crypto');

const jwt = require('jsonwebtoken');

const { promisify } = require('util');

const { Logger } = require('@dojot/microservice-sdk');

const { app: appCfg } = require('../Config');

const RedisManager = require('../Redis/RedisManager');

const jwtSignAsync = promisify(jwt.sign).bind(jwt);

const logger = new Logger();

/**
 * Key in Redis where it is possible to obtain the JWT Token
 * (with the information for the websocket) through the ticket.
 * @param {String} ticket A 'Single Use' ticket.
 */
function getRedisKey(ticket) {
  return `ticket:${ticket}`;
}

/**
 * Obtains the ticket through the Token cryptographic HMAC digests.
 *
 * @param {String} token A JWT Token with the information for the websocket.
 * @returns A ticket calculated via HMAC
 */
function getTicket(token) {
  const hmac = crypto.createHmac('sha256', appCfg.ticket.secret);
  hmac.update(token);
  return hmac.digest('hex');
}

/**
 * Obtains the Token needed to establish a connection to the server via websocket
 *
 * @param {String} ticket Ticket that gives access to the required Token for the websocket.
 */
async function getToken(ticket) {
  const redis = RedisManager.getClient();
  let token = null;
  try {
    token = await redis.getAsync(getRedisKey(ticket));
  } catch (error) {
    logger.error('The token was not found. It may have expired.');
  }
  return token;
}

/**
 * Issue a ticket so that it is possible to establish
 * communication with the server via websocket.
 *
 * @param {Object} auth Object with data extracted from the user's access token.
 */
async function issueTicket({ tenant, expiration }) {
  const tokenJWT = await jwtSignAsync({ tenant, remainingTime: expiration },
    appCfg.ticket.secret,
    { expiresIn: appCfg.ticket.expiresIn });

  const redis = RedisManager.getClient();
  const ticket = getTicket(tokenJWT);

  /* Defines an entry in Redis with a certain expiration time. */
  await redis.client.setexAsync(getRedisKey(ticket), appCfg.ticket.expiresIn, tenant);

  return ticket;
}

module.exports = {
  issueTicket,
  getToken,
};
