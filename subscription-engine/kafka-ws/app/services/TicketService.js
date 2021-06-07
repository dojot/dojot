const crypto = require('crypto');
const jwt = require('jsonwebtoken');
const { promisify } = require('util');
const { ConfigManager, Logger } = require('@dojot/microservice-sdk');
const RedisManager = require('../Redis/RedisManager');

const KAFKA_WS_CONFIG_LABEL = 'KAFKA_WS';
const config = ConfigManager.getConfig(KAFKA_WS_CONFIG_LABEL).ticket;

const jwtSignAsync = promisify(jwt.sign).bind(jwt);

const logger = new Logger('kafka-ws:ticket-service');

/**
 * Key in Redis where it is possible to obtain the JWT Token
 * (with the information for the websocket) through the ticket.
 * @param {String} ticket A 'Single Use' ticket.
 * @returns {String} The Redis key that gives access to the JWT token
 */
function getRedisKey(ticket) {
  return `ticket:${ticket}`;
}

/**
 * Obtains the ticket through the Token cryptographic HMAC digests.
 *
 * @param {String} token A JWT Token with the information for the websocket.
 * @returns {String} A ticket calculated via HMAC
 */
function generateTicket(token) {
  const hmac = crypto.createHmac('sha256', config.secret);
  hmac.update(token);
  return hmac.digest('hex');
}

/**
 * Verifies the integrity and authenticity of the token.
 *
 * @param {String} token Token to verify
 * @param {String} ticket Ticket received
 * @returns {Boolean} If the token is authentic, the ticket received
 *                    and the computed ticket will match.
 */
function verifyToken(token, ticket) {
  const correspondent = generateTicket(token);
  const a = Buffer.from(ticket);
  const b = Buffer.from(correspondent);
  return crypto.timingSafeEqual(a, b);
}

/**
 * Obtains the encoded JWT Token needed to establish a connection to the server via websocket
 *
 * @param {String} ticket Ticket that gives access to the required Token for the websocket.
 * @returns {String} A encoded JWT Token referring to the ticket
 */
async function retrieveEncodedToken(ticket) {
  const redis = RedisManager.getClient();
  try {
    const key = getRedisKey(ticket);
    const multi = redis.multi();
    multi.get(key);
    multi.del(key);
    const exec = promisify(multi.exec).bind(multi);
    const replies = await exec();
    const [token] = replies;

    if (token) {
      if (verifyToken(token, ticket)) {
        return token;
      }
      logger.error(`Failure to verify the integrity and authenticity of the token returned by Redis.
        Obtained Token: ${token}
        Informed Ticket: ${ticket}
      `);
    }
  } catch (error) {
    logger.error(`The token was not found. Ticket ${ticket} has expired or is invalid.`);
  }
  return null;
}

/**
 * Issue a ticket so that it is possible to establish
 * communication with the server via websocket.
 *
 * @param {Object} auth Object with data extracted from the user's access token.
 * @returns {String} A single-use ticket that makes it possible to establish a websocket connection
 */
async function issueTicket({ tenant, expiration }) {
  const token = await jwtSignAsync({ tenant, remainingTime: expiration },
    config.secret,
    { expiresIn: config['expiration.sec'] });

  const redis = RedisManager.getClient();
  const ticket = generateTicket(token);

  /* Defines an entry in Redis with a certain expiration time. */
  const setExAsync = promisify(redis.setex).bind(redis);
  await setExAsync(getRedisKey(ticket), config['expiration.sec'], token);

  return ticket;
}

module.exports = {
  issueTicket,
  retrieveEncodedToken,
};
